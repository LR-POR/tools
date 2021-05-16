#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri May 14 12:43:21 2021

@author: leonel
"""
import re
from os.path import expanduser
sep=re.compile(r"\t|\+")

def extract_entries(infile):
    with open(infile) as f:
        return [line.strip() for line in f.readlines()]
        
def parse_entry(entry):
    return sep.split(entry) 

def build_entry_dict_list(entries):
    entry_dict_list=[]
    for entry in entries:
        entry_dict={}
        parts=parse_entry(entry)
        entry_dict["form"]=parts[0]
        entry_dict["lemma"]=parts[1]
        entry_dict["pos"]=parts[2]
        entry_dict["feats"]=parts[3:]
        entry_dict_list.append(entry_dict)
    return entry_dict_list
    

def build_dict(items,key,value):
    """build a dictionary from items mapping key to value, 
    where value is a list"""
    new_dict={}
    for item in items:
        if new_dict.get(key):
           new_dict[key].append(value) 
        else:
            new_dict[key]=[value]
    return new_dict


def build_lemma_dict(entry_dict_list):
    """build dictionary mapping (lemma,pos) to (form,feats)"""
    lemma_dict={}
    for entry in entry_dict_list:
        key=(entry["lemma"],entry["pos"])
        value=(entry["form"],entry["feats"])
        if lemma_dict.get(key):
            lemma_dict[key].append(value)
        else:
            lemma_dict[key]=[value]
    return lemma_dict

def build_forms_dict(form_feats_list):
    """
    forms=build_forms_dict(lemma_dict['simples','A'])
    forms['simples']
    [['F', 'PL'], ['F', 'SG'], ['M', 'PL'], ['M', 'SG']]
    """
    forms={}
    for form,feats in form_feats_list:
        if not forms.get(form):
           forms[form]=[feats] 
        else:
            forms[form].append(feats)
    return forms
    

def common_feats(feats_lists):
    """
    return a list with the common features of a list of 
    feature lists, e.g.
    
    common_feats([['F', 'PL'], ['F', 'SG'], ['M', 'PL'], ['M', 'SG']])
    
    []
    """
    set_list=[set(feats) for feats in feats_lists]
    for s in set_list[1:]:
        set_list[0].intersection_update(s)
    return list(set_list[0])

def simplify(lemma_dict):
    """
    alegre alegre N [['F', 'SG'], ['M', 'SG']]
alegres alegre N [['F', 'PL'], ['M', 'PL']]

simples [['F', 'PL'], ['F', 'SG'], ['M', 'PL'], ['M', 'SG']]
"""

    for lemma,pos in lemma_dict.keys():
        forms=build_forms_dict(lemma_dict[lemma,pos]) 
        for form in forms.keys():
            feats=forms[form]
            if len(feats) > 1:
                new_feats=common_feats(feats)
            else:
                new_feats=feats[0]
            print(form,lemma,pos,new_feats)
            
def main(infile=expanduser("~/scripts/check-tools/mini.txt")):
    entries=extract_entries(infile)
    entry_dict_list=build_entry_dict_list(entries)
    lemma_dict=build_lemma_dict(entry_dict_list)
    simplify(lemma_dict)
    
if __name__ == "__main__":
    import sys
    main(sys.argv[1])

