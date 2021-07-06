import pandas as pd
import numpy as np
from tqdm.notebook import tqdm
import os
import re
import glob
import pickle


from libs.lda_wrapper import LDA_wrapper
from libs.LastNamesInference import LastNamesInference

class DisciplinesCleaner:
    
    def __init__(self):
        abstracts = pd.read_csv('/data/datasets/WOS/US/abstracts.txt', delimiter='\t')
        titles = pd.read_csv('/data/datasets/WOS/US/titles.txt', delimiter='\t')
        keywords = pd.read_csv('/data/datasets/WOS/US/keywords.txt', delimiter='\t')
        us_papers = pd.read_csv('/data/datasets/WOS/US/US_papers.txt')
        #normalize names of columns
        abstracts.columns = ['id_art', 'Ordre', 'abstract']
        titles.columns = ['id_art', 'ITEMID', 'title']
        keywords.columns = ['Ident', 'id_art', 'Ordre', 'ItemID', 'keywords']
        
        self.abstracts = abstracts
        self.titles = titles 
        self.keywords = keywords
        self.first_authors = self.sample_papers(us_papers)
        
        
    def sample_papers(self, us_papers):
        first_authors = us_papers[us_papers.ordre ==1].copy()

        #1. remove social sciences, humanities, profesional fields and health (and arts because it's too small)
        first_authors = first_authors[-first_authors.EDiscipline.isin(['Health','Professional Fields','Social Sciences','Humanities','Arts'])]

        #2. subsample by discipline, up to 200K articles by field
        big_fields = first_authors[first_authors.EDiscipline.isin(['Clinical Medicine', 'Biomedical Research', 'Engineering and Technology', 'Chemistry', 'Biology'])].groupby("EDiscipline").sample(n=200000, random_state=1234)
        first_authors = first_authors[-first_authors.EDiscipline.isin(['Clinical Medicine', 'Biomedical Research', 'Engineering and Technology', 'Chemistry', 'Biology'])].append(big_fields).reset_index(drop=True)
        return first_authors
    
    
    def create_discipline_text(self, first_authors,discipline):
        discipline_ids = first_authors.loc[first_authors.EDiscipline == discipline,'id_art'].unique()
        discipline_abstracts = self.abstracts[self.abstracts.id_art.isin(discipline_ids)]
        discipline_titles = self.titles[self.titles.id_art.isin(discipline_ids)]
        discipline_keywords = self.keywords[self.keywords.id_art.isin(discipline_ids)]

        discipline_cleaner = Cleaner(discipline_abstracts, discipline_titles, discipline_keywords, stemming=True)

        discipline_text = discipline_cleaner.data_clean()

        discipline_text = discipline_text[['id_art','text_clean']]

        discipline_text.to_csv('/data/datasets/WOS/US/text/text_clean_{}.txt'.format(discipline.replace(' ','_')),index=False)
        
    def main(self):
        
        disciplines = self.first_authors.EDiscipline.unique()
        
        for discipline in tqdm(disciplines, desc="cleaning disciplines"):
            self.create_discipline_text(self.first_authors,discipline)


class LDA_Fields:
    
    def __init__(self,ntopics = 200):
        us_papers = pd.read_csv('/data/datasets/WOS/US/US_papers.txt')
        self.us_papers = us_papers
        self.ntopics = ntopics
        
        
    def save(self, x, file_name):
        with open(file_name, 'wb') as handle:
            pickle.dump(x, handle, protocol=pickle.HIGHEST_PROTOCOL)

    def restore(self, file_name):
        with open(file_name, 'rb') as handle:
            x = pickle.load(handle)
        return x
    
    def fit_model(self,LDA, df,discipline,n_jobs = 64):
        
        if not os.path.exists('../results/lda_fields/lda_model_{}.p'.format(discipline)):
            texts = df['text_clean'].values
            lda_model,data_vectorized,vectorizer = LDA.lda(data=texts,n_components=self.ntopics,n_jobs = 64)

            self.save(lda_model, '../results/lda_fields/lda_model_{}.p'.format(discipline))
            self.save(vectorizer,'../results/lda_fields/vectorizer_{}.p'.format(discipline))
        else:
            lda_model = self.restore( '../results/lda_fields/lda_model_{}.p'.format(discipline))
            vectorizer = self.restore( '../results/lda_fields/vectorizer_{}.p'.format(discipline))
        return lda_model,vectorizer
    
    def transform_data(self, df,lda_model,vectorizer):
        texts = df.text_clean.values
        data_vectorized = vectorizer.transform(texts)
        doc_dist = lda_model.transform(data_vectorized)
        return data_vectorized, doc_dist

    def infer_race(self, us_papers,df):
        
        papers = us_papers.loc[(us_papers.id_art.isin(df.id_art)),]
        first_authors = papers[papers.ordre==1].copy().reset_index(drop=True)
        
        lni = LastNamesInference(names = first_authors.nom)
        tqdm.pandas(desc="inferring race from lastnames")
        lastname_race_dist = first_authors.progress_apply(lambda x: lni.get_name_dist(lastname=x.nom), axis=1)
        first_authors[lni.prob_order] = pd.DataFrame(lastname_race_dist.to_list())
        df_race = df.merge(first_authors, on ='id_art')
        return df_race
    
    def project_lda_topics(self, df_race,doc_dist):

        race_dist = df_race.filter(regex=('white|hispanic|black|asian'))
        topics_by_group = race_dist.T @ doc_dist 

        topics_by_group = topics_by_group.astype(np.float128)

        joint_prob = topics_by_group/topics_by_group.to_numpy().sum()
        marginal_by_topic = joint_prob.div(joint_prob.sum(axis=0), axis=1)
        marginal_by_group = joint_prob.div(joint_prob.sum(axis=1), axis=0)
        dist_diff_topic = marginal_by_topic.div(joint_prob.sum(axis=1), axis=0) -1   
        
        joint_prob = joint_prob.T.rename_axis('topic').reset_index()
        marginal_by_topic = marginal_by_topic.T.rename_axis('topic').reset_index()
        marginal_by_group = marginal_by_group.T.rename_axis('topic').reset_index()
        dist_diff_topic = dist_diff_topic.T.rename_axis('topic').reset_index()

        # I start the topics in 1, so they are equal to the LDAVIZ!!!!
        joint_prob.topic += 1
        marginal_by_topic.topic += 1
        marginal_by_group.topic += 1
        dist_diff_topic.topic += 1    

        return joint_prob, marginal_by_topic, marginal_by_group, dist_diff_topic
    
    def intersect_by_gender(self, df_race, doc_dist):

        df_race['gender'] = df_race.gender.str.upper()

        boolean_mask_M = df_race.gender == 'M'
        boolean_mask_F = df_race.gender == 'F'

        race_dist_M = df_race.loc[boolean_mask_M,['white','hispanic','black','asian']]
        race_dist_F = df_race.loc[boolean_mask_F,['white','hispanic','black','asian']]

        race_dist_M.columns = race_dist_M.columns + '_M'
        race_dist_F.columns = race_dist_F.columns + '_F'

        race_dist_MF = pd.concat([race_dist_M,race_dist_F]).fillna(0)
        doc_dist_MF = np.concatenate((doc_dist[boolean_mask_M],doc_dist[boolean_mask_F]))

        joint_prob, marginal_by_topic, marginal_by_group, dist_diff_topic = self.project_lda_topics(race_dist_MF,doc_dist_MF)

        return joint_prob, marginal_by_topic, marginal_by_group, dist_diff_topic
    
    def save_statistics(self, discipline,df_race, doc_dist):
        
        joint_prob_gender, marginal_by_topic_gender, marginal_by_group_gender, dist_diff_topic_gender = self.intersect_by_gender(df_race, doc_dist)

        joint_prob_gender.to_csv('../results/lda_fields/joint_prob_{}.csv'.format(discipline),index=False)
        marginal_by_topic_gender.to_csv('../results/lda_fields/marginal_by_topic_{}.csv'.format(discipline),index=False)
        marginal_by_group_gender.to_csv('../results/lda_fields/marginal_by_group_{}.csv'.format(discipline),index=False)
        dist_diff_topic_gender.to_csv('../results/lda_fields/dist_diff_topic_{}.csv'.format(discipline),index=False)

    def top_words(self, topic):
        tw = topic[topic.values.argsort()][:-5 - 1:-1].index
        tw = ', '.join(tw.to_list())
        return tw 
    
    def save_top_words(self,LDA, discipline,lda_model,vectorizer):
        words_by_topic = LDA.topic_keyowrd_matrix(lda_model,vectorizer)
        tws = words_by_topic.apply(lambda row: self.top_words(row),axis=1)
        top_words_df = pd.DataFrame(tws.values, columns=['top_words'])
        top_words_df['topic'] = range(self.ntopics)
        top_words_df.topic += 1
        top_words_df.to_csv('../results/lda_fields/top_words_{}.csv'.format(discipline), index=False)

    def save_topics_proportion(self,discipline, doc_dist):
        topic_proportion = pd.DataFrame(doc_dist.sum(axis=0)/np.sum(doc_dist))
        topic_proportion.columns = ['proportion']
        topic_proportion['topic'] = topic_proportion.index +1
        topic_proportion.to_csv('../results/lda_fields/topic_proportion_{}.csv'.format(discipline),index=False)

    def calculate_single_discipline(self,discipline,path,n_batches=50):
        df = pd.read_csv(path)
        LDA = LDA_wrapper(n_batches=n_batches, logname='_all_fields')
        print(discipline)
        lda_model,vectorizer = self.fit_model(LDA, df,discipline,64)
        df_race = self.infer_race(self.us_papers,df)

        data_vectorized, doc_dist = self.transform_data(df_race,lda_model,vectorizer)

        self.save_statistics(discipline,df_race, doc_dist)
        self.save_top_words(LDA, discipline,lda_model,vectorizer)
        self.save_topics_proportion(discipline, doc_dist)

    def fit_all_fields(self,main_path ="/data/datasets/WOS/US/text/sample/"):
    
        filenames =  glob.glob(main_path + '*.txt')
        disciplines = [os.path.basename(x).replace(r'text_clean_', '').replace('.txt','') for x in filenames]  
        for discipline,path in tqdm(zip(disciplines,filenames)):
            self.calculate_single_discipline(discipline,path, n_batches=50)   