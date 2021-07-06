import unidecode
import pickle
import numpy as np
import os, sys
import re
from tqdm import tqdm
    
CURRENT_DIR = os.path.dirname(os.path.abspath(__file__)) + '/'

class LastNamesInference:
    """
    This class takes Last name and assign a vector of probabilities for each racial group
    """
    def __init__(self, names):
        
        dictionaries = pickle.load(open(CURRENT_DIR+'names_prob.p', 'rb'))
        self.impute_by_mean(names, lastnames_dict = dictionaries['lastnames_dict'])     
        self.prob_order = ['white','hispanic','black','asian']
        
    def impute_by_mean(self,names,lastnames_dict):
        
        lastnames_dict['ALL OTHER LAST NAMES'] = np.array([0,0,0,0])  #remove unknown names
        self.lastnames_dict = lastnames_dict
        tqdm.pandas(desc="imputing by the mean")
        known_names_dist = names.progress_apply(lambda x: self.get_name_dist(x)).values

        self.known_names_dist = known_names_dist
        mean_dist = known_names_dist.mean()/known_names_dist.mean().sum()
        
        lastnames_dict['ALL OTHER LAST NAMES'] = mean_dist #impute with mean distribution        
        self.lastnames_dict = lastnames_dict
        #return(lastnames_dict)
    
    def clean_nom(self, nom):
        #remove accents
        nom = unidecode.unidecode(nom)
        
        #keep only first part until space
        nom = nom.split(' ',1)[0]
        
        nom = re.sub(r"-[A-Z]+",'',nom)
        nom = re.sub(' +', ' ',nom)
        nom = nom.strip()
        return nom.upper()
    
    def get_name_dist(self,lastname):
        
        lastname = self.clean_nom(lastname)
        
        if lastname not in self.lastnames_dict.keys():
            lastname = 'ALL OTHER LAST NAMES'
        
        dist = self.lastnames_dict[lastname]
        return(dist)