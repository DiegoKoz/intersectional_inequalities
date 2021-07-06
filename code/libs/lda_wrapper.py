from sklearn.model_selection import GridSearchCV
from sklearn.decomposition import LatentDirichletAllocation
from sklearn.feature_extraction.text import CountVectorizer
import pandas as pd
import pyLDAvis.gensim
import pyLDAvis.sklearn
import pickle
import logging
import os
import numpy as np
import glob
from scipy.sparse import vstack

class LDA_wrapper():

    def __init__(self,n_batches=100, log_level= logging.INFO, logname=''):
               
        logging.basicConfig(filename='log'+logname, filemode="w",format='%(asctime)s %(message)s', datefmt='%d/%m/%Y %H:%M:%S',level=log_level)
        logger = logging.getLogger()
        logger.setLevel(logging.DEBUG)
        self.logger = logger
        if not os.path.exists('tmp/'):
            self.logger.info('creating tmp path: {}'.format('tmp/'))
            os.makedirs('tmp/')
        self.n_batches = n_batches

    def save_batches(self,X):
        indexes = np.array_split(np.array(range(X.shape[0])),self.n_batches)
        batches = [X[i] for i in indexes]
        for i in range(self.n_batches):
            file_path = 'tmp/batch_{}.p'.format(i)
            self.save(batches[i], file_path)
            
    def load_batches(self):
        batches_path = ['tmp/batch_{}.p'.format(i) for i in range(self.n_batches)]
        batch_data = list()
        for batch in batches_path:
            btc = self.restore(batch)
            batch_data.append(btc)
        batch_data = vstack(batch_data)
        return batch_data


    def data_vectorizer(self, data, lowercase=True, ngram_range = (1, 2), min_df=100, max_df=.65, token_pattern='[a-zA-Z][a-zA-Z]{2,}'):
        self.logger.info('vectorizing')
        vectorizer = CountVectorizer(lowercase=lowercase, 
                                     min_df=min_df,
                                     max_df=max_df,
                                     ngram_range = ngram_range,
                                     token_pattern=token_pattern,
                                     strip_accents = 'ascii',
                                     stop_words = 'english')

        data_vectorized = vectorizer.fit_transform(data)
        self.logger.info('finish vectorizing')
        self.vectorizer = vectorizer
        self.save(vectorizer, 'tmp/vectorizer.p')
        self.save(data_vectorized, 'tmp/data_vectorized.p')
        return data_vectorized, vectorizer
    
    def save(self, x, file_name):
        with open(file_name, 'wb') as handle:
            pickle.dump(x, handle, protocol=pickle.HIGHEST_PROTOCOL)

    def restore(self, file_name):
        with open(file_name, 'rb') as handle:
            x = pickle.load(handle)
        return x

    def lda(self, data, n_components=None, max_iter=100, learning_method='online', n_jobs = 64, min_df=100, max_df=.65, random_state =1234):
        
        lda_model = LatentDirichletAllocation(n_components=n_components,
                                              max_iter=max_iter,
                                              learning_method=learning_method,
                                              verbose=1,
                                              random_state=random_state,
                                             n_jobs = n_jobs)
        
        data_vectorized, vectorizer = self.data_vectorizer(data, min_df= min_df,max_df=max_df)
#         data_vectorized, _ = self.data_vectorizer(data, min_df= min_df,max_df=max_df)
        #lda_model.fit(data_vectorized)
        self.save_batches(data_vectorized)
        
#         del data_vectorized
        
        tmp_file = 'tmp/partial_lda.p'
        self.save(lda_model,tmp_file)
        for i in range(self.n_batches):
            #load batch
            batch_path = 'tmp/batch_{}.p'.format(i)
            batch = self.restore(batch_path)
            #load model
            lda_model = self.restore(tmp_file)
            self.logger.info('start batch {}/{}'.format(i,self.n_batches))
            #fit model
            lda_model.partial_fit(batch)
            self.logger.info('finish batch {}/{}'.format(i,self.n_batches))
            #save model
            self.save(lda_model,tmp_file)
            del batch
        self.lda_model = lda_model
        return lda_model,data_vectorized,vectorizer

    def lda_vis(self, lda_model, data_vectorized, vectorizer, method='tsne'):

        pyLDAvis.enable_notebook()
        vis = pyLDAvis.sklearn.prepare(lda_model, data_vectorized, vectorizer, mds=method,sort_topics=False )
        return vis

    def topic_keyowrd_matrix(self, lda_model, vectorizer):

        topicnames = ["Topic" + str(i) for i in range(lda_model.n_components)]

        # Topic-Keyword Matrix
        df_topic_keywords = pd.DataFrame(lda_model.components_)

        # Assign Column and Index
        df_topic_keywords.columns = vectorizer.get_feature_names()
        df_topic_keywords.index = topicnames
        return df_topic_keywords

    def display_topics(self, model=None, feature_names=None, no_top_words=10, subset=None):

        if model is None:
            model = self.lda_model

        if feature_names is None:
            feature_names = self.vectorizer.get_feature_names()

        if subset is None:
            subset = range(model.components_.shape[0])
        for i in subset:
            topic = model.components_[i]
            print("Topic %d:" % (i))
            print(" ".join([feature_names[j] for j in topic.argsort()[:-no_top_words - 1:-1]]))

