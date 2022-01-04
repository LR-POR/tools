#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author: Leonel Figueiredo de Alencar 
# Date: Dec. 21, 2021

"""This module extracts verbs from the UD Bosque treebank and constructs the corresponding lexical entries in TDL according to the given tables mapping UD valence frames to PorGram types.

de Alencar, Leonel Figueiredo; Coutinho, Lucas Ribeiro; da Silva, Wellington José Leite; Nunes, Ana Luiza; Rademaker, Alexandre. Extracting valences from a dependency treebank for populating the verb lexicon of a Portuguese HPSG grammar. Proceedings of the 15th International Conference on Computational Processing of Portuguese (PROPOR 2022). Berlin: Springer, 2022. In preparation.
"""

import sys, os, re, pickle
USER=os.path.expanduser("~")
sys.path.append(os.path.join(USER, "scripts"))
import PorGramEntries, ValenceExtractor
from valences import *
DIR1=os.path.join(USER, "hpsg/por")
DIR2=os.path.join(USER, "hpsg/por/tmp")
# list of the names of the files defining the mapping from UD valence frames to PorGram types 
FILENAMES=['prep-obj-2ng-arg-verbs','clausal-verbs', 'intrans-trans', 'ditrans']
# destination file of the created entries
OUTFILE="bosque-entries.tdl"

def build_mapping(filename):
    return dict([re.split(r"\s+",line.strip()) for line in open(os.path.join(USER,DIR1,filename),"r").readlines() if line.strip() != ""])

#MAPPING=build_mapping()

# table mapping lexical identifiers to types in the existing TDL lexicon files 
INFILE=os.path.join(USER, DIR2,"verbtypes142256.txt")

LEXICON=PorGramEntries.MakeDictionary(PorGramEntries.ExtractEntries(INFILE))

def save_lexicon(lexicon,filename,path_to_dir=DIR1):
    outfile = open(os.path.join(path_to_dir,filename),'wb')
    pickle.dump(lexicon,outfile)
    outfile.close()
    
def load_lexicon(filename,path_to_dir=DIR1):
    outfile = open(os.path.join(path_to_dir,filename),'rb')
    lexicon=pickle.load(outfile)
    outfile.close()
    return lexicon

def from_frames_to_types(filenames=FILENAMES):
    mapping={}
    for filename in filenames:
        mapping.update(build_mapping(f"{filename}.txt"))
    return mapping

def build_entry_old(lemma,verb_type,dic=LEXICON):
	verb_types=dic.get(lemma)
	if verb_types:
		if verb_type not in verb_types:
			ind=len(verb_types)+1
		else:
			return None
	else:
		ind=1
	return f"""{lemma}_{ind} := {verb_type} &
  [ STEM < "{lemma}" >,
    SYNSEM.LKEYS.KEYREL.PRED "_{lemma}_v_{ind}_rel" ].\n"""
 
def build_entry(lemma,verb_type,index):
    return f"""{lemma}_{index} := {verb_type} &
  [ STEM < "{lemma}" >,
    SYNSEM.LKEYS.KEYREL.PRED "_{lemma}_v_{index}_rel" ].\n"""
  
def expand_lexicon(verb,verb_type,new_lexicon):
    if not new_lexicon.get(verb.lemma):
            new_lexicon[verb.lemma]={verb_type}
    else:
        new_lexicon[verb.lemma].add(verb_type)
  
def build_lexicon(mapping,dative=True):
    frames=list(mapping.keys())
    new_lexicon={}
    for frame in frames:
        framelist=ValenceExtractor.expand_valence(frame,dative)
        verbs=ValenceExtractor.extract_verbs(framelist)
        for verb in verbs:
            expand_lexicon(verb,mapping[frame],new_lexicon)
    return new_lexicon

def join_lexicons(*lexicons):
    old_lexicon=lexicons[0]
    new_lexicons=lexicons[1:]
    updated_lexicon={}
    updated_lexicon.update(old_lexicon)
    #return new_lexicons
    for lexicon in new_lexicons:
        for k,v in lexicon.items():
            if updated_lexicon.get(k):
                updated_lexicon[k].update(v)
            else:
                updated_lexicon[k]=v
    return updated_lexicon

def collapse_dat_goa(lexicon):
    dat_goa={'nom-acc-dat-ditransitive-verb-lex','nom-acc-goa-ditransitive-verb-lex'}
    for lemma,verbtypes in lexicon.items():
        if verbtypes.issuperset(dat_goa):
            verbtypes.difference_update(dat_goa)
            verbtypes.add('nom-acc-rec-ditransitive-verb-lex')
			
def write_lexicon(new_lexicon,path_to_dir=DIR1, outfile=OUTFILE,old_lexicon=LEXICON):
    outfile=os.path.join(path_to_dir,outfile)
    outfile=open(outfile,"w")
    lemmas=list(new_lexicon.keys())
    for lemma in lemmas:
        index=1
        old_verbtypes=old_lexicon.get(lemma)
        new_verbtypes=new_lexicon.get(lemma)
        if old_verbtypes:
            index=len(old_verbtypes)+1
            for verbtype in new_verbtypes:
                if verbtype not in old_verbtypes:
                    print(build_entry(lemma,verbtype,index),file=outfile)
                    index+=1
        else:
            for verbtype in new_verbtypes:
                print(build_entry(lemma,verbtype,index),file=outfile)
                index+=1
    outfile.close()
 
def build_lexicons(filenames=FILENAMES):
    new_lexicons=[]
    for filename in filenames:
        infile=f"{filename}.txt"
        outfile=f"{filename}.pkl"
        mapping=build_mapping(infile)
        new_lexicon=build_lexicon(mapping)
        #save_lexicon(new_lexicon,outfile)
        new_lexicons.append(new_lexicon)
    return new_lexicons
        
def main(filenames=FILENAMES,outfile=OUTFILE):
    lexicons_list=build_lexicons(filenames)
    new_lexicon=join_lexicons(*lexicons_list)
    collapse_dat_goa(new_lexicon)
    write_lexicon(new_lexicon,outfile=outfile)
		

if __name__ == "__main__":
    if len(sys.argv) > 1:
        main(sys.argv[1:])
    else:
        main()
