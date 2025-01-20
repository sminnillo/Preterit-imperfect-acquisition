#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 22 10:25:19 2021

@author: sophiaminnillo
"""

##CEDEL2 for preterit-imperfect project

#%%
# imports
# import pickle
import pickle
#import pandas
import pandas as pd
#import os
import os

#%%

### PART 1: L2 DATA ###

#set wd

path="/Users/sophiaminnillo/Box/pythonProject/CorpusResearch/CEDEL2_data"
os.chdir(path)
os.getcwd()
#%%
##Essays
# load pickle files

#corpora just as regular lists

#region where you live
region_corpus = pickle.load(open("C_essays_region_tagged.pickle", "rb"))
#famous person
famous_corpus = pickle.load(open("C_essays_famous_tagged.pickle", "rb"))
#film
film_corpus = pickle.load(open("C_essays_film_tagged.pickle", "rb"))
#last year holiday
holiday_corpus = pickle.load(open("C_essays_holiday_tagged.pickle", "rb"))
#future plans
future_corpus = pickle.load(open("C_essays_future_tagged.pickle", "rb"))
#recent trip
vacation_corpus = pickle.load(open("C_essays_vacation_tagged.pickle", "rb"))
#experience
experience_corpus = pickle.load(open("C_essays_experience_tagged.pickle", "rb"))
#terrorism
terrorism_corpus = pickle.load(open("C_essays_terrorism_tagged.pickle", "rb"))
#anti-smoking law
smoking_corpus = pickle.load(open("C_essays_smoking_tagged.pickle", "rb"))
#gay couples
gay_corpus = pickle.load(open("C_essays_gay_tagged.pickle", "rb"))
#marajuana legalization
weed_corpus = pickle.load(open("C_essays_weed_tagged.pickle", "rb"))
#immigration
immigration_corpus = pickle.load(open("C_essays_immigration_tagged.pickle", "rb"))
#frog
frog_corpus = pickle.load(open("C_essays_frog_tagged.pickle", "rb"))
#chaplin
chaplin_corpus = pickle.load(open("C_essays_chaplin_tagged.pickle", "rb"))
#everything else I had missed before
final_corpus = pickle.load(open("C_essays_final_tagged.pickle", "rb"))

"""
Nested tuple/list with:
1. (ID, [everything else])
2. Tagged essay separated in sentences (every sentence is a list of tuples)
3. Each tuple has three elements: original word, lemmatized word, part-of-speech tag
"""

#%%
#all the data in nice  lists
#corpus list
corpus = [region_corpus, famous_corpus, film_corpus, holiday_corpus,
          future_corpus, vacation_corpus, experience_corpus, terrorism_corpus,
          smoking_corpus, gay_corpus, weed_corpus, immigration_corpus, 
          frog_corpus, chaplin_corpus, final_corpus]

#%%
#reset wd

path1="/Users/sophiaminnillo/Box/pythonProject/CorpusResearch"
os.chdir(path1)
os.getcwd()
#%%
##Metadata
##These are the essays we want to analyze
meta = pd.read_csv('CEDEL2_L2_metadata_042221.csv') 

#%%
#just filenames
need_data = meta[['Filename']] 
#list of lists by participant
need_data_list = need_data.values.tolist()

#metadata with lowercase file names to match with essays
need_data_list1 = []

for essay_info in need_data_list:
    ID = essay_info[0]
    ID1 = ID.lower() #had to make lowercase to match with essay
    need_data_list1.append(ID1)
    
#%%
#let's basically just get a corpus of only the relevant essays
#the Filename is the key to match the participant with
#their essay

new_corpus = []
for real_person in need_data_list1:
    for prompt in corpus:
        for essay in prompt:
            ID = essay[0]
            tags = essay[1]
            if ID == real_person:
                new_corpus.append(essay)

#%%
#function to use to link metadata with essay info
def DFofCounts(ID_info, fxn, corpus):
    alist = []
    
    for a in ID_info:
        ID = a[0]
        b = fxn(ID, corpus)
        alist.append(b) 
    return alist  
#%%
#extract frequency of all verbs in preterite & imperfect by lemma
def IMPVerbs(ID, corpus):
    
    list_of_words_AWL = []
    
    for essay in corpus: #going through each essay
        name = essay[0]
        if name == ID: #if the ID matches the order of IDs we've established
            ind_corpus = essay[1]
            for sentence in ind_corpus:
                for word in sentence:
                    lemma = word[1] #looking at LEMMA
                    pos_tag = word[2]
                    if pos_tag[0] == 'V' and pos_tag[2] == 'I' and pos_tag[3] == 'I':
                        #verb, indicative mood, imperfect tense
                        list_of_words_AWL.append(lemma)
                 
                    
    return list_of_words_AWL


#%%
#extract frequency of all verbs in imperfect by token

def IMPVerbsOG(ID, corpus):
    
    list_of_words_AWL = []
    
    for essay in corpus: #going through each essay
        name = essay[0]
        if name == ID: #if the ID matches the order of IDs we've established
            ind_corpus = essay[1]
            for sentence in ind_corpus:
                for word in sentence:
                    OG = word[0] #token
                    pos_tag = word[2]
                    if pos_tag[0] == 'V' and pos_tag[2] == 'I' and pos_tag[3] == 'I':
                        #verb, indicative mood, imperfect tense
                        list_of_words_AWL.append(OG)                    

                    
    return list_of_words_AWL



#%%
#extract frequency of all verbs in preterit by lemma
def PRETVerbs(ID, corpus):
    
    list_of_words_AWL = []
 
    for essay in corpus: #going through each essay
        name = essay[0]
        if name == ID: #if the ID matches the order of IDs we've established
            ind_corpus = essay[1]
            for sentence in ind_corpus:
                for word in sentence:
                    lemma = word[1] #lemma
                    pos_tag = word[2]
                    if pos_tag[0] == 'V' and pos_tag[2] == 'I' and pos_tag[3] == 'S':
                        #verb, indicative mood, preterit
                        list_of_words_AWL.append(lemma)  
    
    return list_of_words_AWL
                        

#%%
#extract frequency of all verbs in preterite by token

def PRETVerbsOG(ID, corpus):
    
    list_of_words_AWL = []
 
    for essay in corpus: #going through each essay
        name = essay[0]
        if name == ID: #if the ID matches the order of IDs we've established
            ind_corpus = essay[1]
            for sentence in ind_corpus:
                for word in sentence:
                    OG = word[0] #token
                    pos_tag = word[2]
                    if pos_tag[0] == 'V' and pos_tag[2] == 'I' and pos_tag[3] == 'S':
                        #verb, indicative mood, preterit
                        list_of_words_AWL.append(OG)                     
                    
    return list_of_words_AWL


#%%
#function to transform the forms extracted into a csv file
def FormstoCSV(fxn, corpus_name, meta_df, csv_name): #csv_name must be string
   
   #possible functions (fxn):
       #preterit tokens: PRETVerbsOG
       #imperfect tokens: IMPVerbsOG
       #preterit lemmas: PRETVerbsLemma
       #imperfect lemmas: IMPVerbsLemma
    
   #get forms in the order of the metadata
   a = DFofCounts(corpus_name, fxn, corpus_name)
   
   #turn into df
   b = pd.DataFrame(a)
   
   #add metadata
   c = pd.concat([meta_df, b], axis=1) #making upper case again
   
   #export to csv
   c.to_csv(csv_name, index=True)
   
   return c

#%%
#preterit tokens: PRETVerbsOG
pret_tokens_df = FormstoCSV(PRETVerbsOG, new_corpus, need_data, 'CEDEL2_pret_tokens_042221.csv')
#imperfect tokens: IMPVerbsOG
imp_tokens_df = FormstoCSV(IMPVerbsOG, new_corpus, need_data, 'CEDEL2_imp_tokens_042221.csv')
#preterit lemmas: PRETVerbs
pret_lemmas_df = FormstoCSV(PRETVerbs, new_corpus, need_data, 'CEDEL2_pret_lemmas_042221.csv')
#imperfect lemmas: IMPVerbs
imp_lemmas_df = FormstoCSV(IMPVerbs, new_corpus, need_data, 'CEDEL2_imp_lemmas_042221.csv')

#%%
### PART 2: NS DATA ###

#set wd

path2="/Users/sophiaminnillo/Box/pythonProject/CorpusResearch/CEDEL2_data/CEDEL2_NS"
os.chdir(path2)
os.getcwd()
#%%
##Essays
# load pickle files

#corpora just as regular lists
NS1_corpus = pickle.load(open("C_essays_NS1_tagged.pickle", "rb"))
NS2_corpus = pickle.load(open("C_essays_NS2_tagged.pickle", "rb"))

"""
Nested tuple/list with:
1. (ID, [everything else])
2. Tagged essay separated in sentences (every sentence is a list of tuples)
3. Each tuple has three elements: original word, lemmatized word, part-of-speech tag
"""
#%%
#all the data in nice  lists
#corpus list
corpus_NS = [NS1_corpus, NS2_corpus]
#%%
#reset wd

path1="/Users/sophiaminnillo/Box/pythonProject/CorpusResearch"
os.chdir(path1)
os.getcwd()
#%%
##Metadata
##These are the essays we want to analyze
meta_NS = pd.read_csv('CEDEL2_NS_metadata_042221.csv') 
#820

#%%
#just filenames
need_data_NS = meta_NS[['Filename']] 
#list of lists by participant
need_data_list_NS = need_data_NS.values.tolist()

#metadata with lowercase file names to match with essays
need_data_list1_NS = []

for essay_info in need_data_list_NS:
    ID = essay_info[0]
    ID1 = ID.lower() #had to make lowercase to match with essay
    need_data_list1_NS.append(ID1)
    
#%%
#let's just get a corpus of only the relevant essays
new_corpus_NS = []
for real_person in need_data_list1_NS:
    for prompt in corpus_NS:
        for essay in prompt:
            ID = essay[0]
            tags = essay[1]
            if ID == real_person:
                new_corpus_NS.append(essay)
                
#%%
##running all of the functions for NS corpora and metadata

#preterit tokens: PRETVerbsOG
pret_tokens_df_NS = FormstoCSV(PRETVerbsOG, new_corpus_NS, need_data_NS, 'CEDEL2_NS_pret_tokens_042221.csv')
#imperfect tokens: IMPVerbsOG
imp_tokens_df_NS = FormstoCSV(IMPVerbsOG, new_corpus_NS, need_data_NS, 'CEDEL2_NS_imp_tokens_042221.csv')
#preterit lemmas: PRETVerbs
pret_lemmas_df_NS = FormstoCSV(PRETVerbs, new_corpus_NS, need_data_NS, 'CEDEL2_NS_pret_lemmas_042221.csv')
#imperfect lemmas: IMPVerbs
imp_lemmas_df_NS = FormstoCSV(IMPVerbs, new_corpus_NS, need_data_NS, 'CEDEL2_NS_imp_lemmas_042221.csv')



