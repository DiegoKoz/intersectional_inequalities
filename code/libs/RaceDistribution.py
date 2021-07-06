import unidecode
import pickle
import numpy as np
import os, sys

CURRENT_DIR = os.path.dirname(os.path.abspath(__file__)) + '/'

class RaceDistribution:
    """
    This class takes a First and Last name and assign a vector of probabilities for each ethnic group
    """
    def __init__(self,exp=2,mode='sd', normalized_firstnames = None):
        """"
        exp: int. Default:1. Exponential to use
        mode: str: {'sd','lastname','name'}: Default: sd. Define whether to use only lastname, name or the sd-based average
        """
        
        dictionaries = pickle.load(open(CURRENT_DIR+'names_prob.p', 'rb'))
        self.lastnames_dict = dictionaries['lastnames_dict']
        self.firstnames_dict = dictionaries['firstnames_dict']
        self.firstnames_norm_dict = dictionaries['firstnames_norm_dict']
        self.exp = exp
        self.mode = mode
        self.normalized_firstnames = normalized_firstnames
        
        self.prob_order = ['white','hispanic','black','asian','other']
        
    def weighting_scheme(self,name, lastname, exp=2):

        name_sd = np.std(name)**exp
        lastname_sd = np.std(lastname)**exp

        total = name_sd+lastname_sd

        if (total==0):
            name_sd = lastname_sd = 1
            total=2

        name_weight = name_sd/total
        lastname_weight = lastname_sd/total
        return({'name_weight':name_weight,'lastname_weight':lastname_weight})
    
    def weighted_distribution(self,name, lastname,exp):     
        weights = self.weighting_scheme(name, lastname, exp=exp)
        return(name*weights['name_weight'] + lastname*weights['lastname_weight'])
    
    def get_names_dist(self,lastname,name=None):
        
        #remove accents
        name = unidecode.unidecode(name)
        lastname = unidecode.unidecode(lastname)
        
        #keep only first part until space
        name = name.split(' ', 1)[0]
        lastname = lastname.split(' ',1)[0]
        
        # to uppercase
        name = name.upper()
        lastname = lastname.upper()

        # Select firstname type
        if self.normalized_firstnames:
            firstnames_dict = self.firstnames_norm_dict
        else:
            firstnames_dict= self.firstnames_dict


        if name not in self.firstnames_dict.keys():
            name = 'ALL OTHER FIRST NAMES'

        if lastname not in self.lastnames_dict.keys():
            lastname = 'ALL OTHER LAST NAMES'
        
        if self.mode == 'lastname':
            new_dist = self.lastnames_dict[lastname]

        if self.mode == 'name':
            new_dist = firstnames_dict[name]
        
        if self.mode == 'sd':
            new_dist = self.weighted_distribution(firstnames_dict[name], self.lastnames_dict[lastname],exp=self.exp)

        return(new_dist)