from tqdm.notebook import tqdm
from nltk.corpus import stopwords
from nltk.stem import SnowballStemmer
import unicodedata2
import string
import os

CURRENT_DIR = os.path.dirname(os.path.abspath(__file__)) + '/'

class Cleaner:
    
    def __init__(self,abstracts, titles, keywords, stemming=True):
        
        
        with open(CURRENT_DIR+"common_phrases.txt", 'r') as r_s_files:
            common_phrases=[line.replace('\n', '') for line in r_s_files.readlines()]

        common_phrases = [self.clean_common_phrases(phrase) for phrase in common_phrases]

        self.common_phrases = common_phrases    
        
        abstracts[["abstract"]] = abstracts[["abstract"]].astype(str) 
        titles[["title"]] = titles[["title"]].astype(str) 
        keywords[["keywords"]] = keywords[["keywords"]].astype(str) 
  
        abstracts = self.collapse_text(abstracts,'abstract')
        titles = self.collapse_text(titles, 'title')
        keywords = self.collapse_text(keywords,'keywords')
        
        print('we have {} abstracts-articles, {} titles-articles and {} keywords-articles'.format(len(set(abstracts.id_art)),len(set(titles.id_art)),len(set(keywords.id_art))))
        
        self.abstracts = abstracts
        self.titles = titles
        self.keywords = keywords
        self.stemming = stemming 

        self.stemming_dict_ = {}
        
        
    def clean_common_phrases(self,phrase):
        phrase = phrase.lower()            
        remove_punctuation_map = dict((ord(char), ' ') for char in string.punctuation)
        phrase = self.strip_accents(phrase)
        phrase = phrase.translate(remove_punctuation_map)
        return phrase

    def collapse_text(self, df, text):
        tqdm.pandas()
        return df.groupby('id_art')[text].progress_apply(' '.join).reset_index()
    
    def strip_accents(self, text):
        text = unicodedata2.normalize('NFD', text)
        text = text.encode('ascii', 'ignore')
        text = text.decode("utf-8")
        return str(text)
    
    def stemmer(self, filtered_words):
        stemmer = SnowballStemmer('spanish')
        stemmed_words = []
        for i in filtered_words:
            stemmed = stemmer.stem(i)
            stemmed_words.append(stemmed)
            if stemmed in self.stemming_dict_:
                dict_stem = self.stemming_dict_[stemmed]
                if i in dict_stem:
                    dict_stem[i] =  dict_stem[i] + 1
                else:
                    dict_stem[i] = 1
            else:
                 self.stemming_dict_[stemmed] = {i:1}
        doc = (' ').join(stemmed_words)
        return  doc
    
    def de_stemmer(self, data):
        #busco el mas frecuente
        replacement = {}
        for key in tqdm(self.stemming_dict_.keys(), desc="select most representative word of stem"):
            stem_dict = self.stemming_dict_[key]
            replacement[key] = max(stem_dict, key=stem_dict.get)
        self.replacement = replacement
        #remplazo en los documentos
        de_stemmed = []
        for doc in tqdm(data, desc = "de-stemming"):
            word_list = doc.split(' ')
            new_list = []
            for word in word_list:
                if word in self.stemming_dict_:
                    new_list.append(replacement.get(word))
                else:
                    new_list.append(word)
            de_stemmed.append(' '.join(new_list))
        return de_stemmed
    
    
    def preprocess(self, doc,stem, stopwords_lang = "english"):
        doc = doc.lower()            
        remove_punctuation_map = dict((ord(char), ' ') for char in string.punctuation)
        doc = self.strip_accents(doc)
        doc = doc.translate(remove_punctuation_map)
        #remove common phrases from common_phrases.txt
        for phrase in self.common_phrases:
            doc=doc.replace(phrase, '')
        
        remove_words  = stopwords.words(stopwords_lang)
        querywords = doc.split()
        #Remove numbers
        querywords = [" " if c.isdigit() else c for c in querywords]
        filtered_words = [palabra for palabra in querywords if palabra not in remove_words]       
        if stem:
            doc = self.stemmer(filtered_words)
        else:
            doc = (' ').join(filtered_words)
        return doc
    
    def data_clean(self, n_abstract=1, n_title=3, n_keywords=3):
        
        text = self.abstracts.merge(self.titles,how='left', on = 'id_art')
        text = text.merge(self.keywords,how='left', on = 'id_art')

        text = text.loc[:,['id_art','abstract','title','keywords']]
        text.fillna('',inplace=True)
        text['text'] = text.apply(lambda x: n_abstract*(str(x.abstract) +' ')+ n_title*(str(x.title) +' ')+ n_keywords*(str(x.keywords) + ' '), axis=1)
        
        text_clean = [self.preprocess(doc, stem = self.stemming) for doc in tqdm(text['text'], desc = "processing")]
        
        if self.stemming:
            text_clean = self.de_stemmer(text_clean)
        
        text['text_clean'] = text_clean
        return text    
