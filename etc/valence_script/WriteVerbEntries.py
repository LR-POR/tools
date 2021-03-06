#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author: Leonel Figueiredo de Alencar 
# Date: Dec. 21, 2021

"""This module extracts verbs from the UD Bosque treebank and constructs the corresponding lexical entries in TDL according to the given tables mapping UD valence frames to PorGram types.

de Alencar, Leonel Figueiredo; Coutinho, Lucas Ribeiro; da Silva, Wellington José Leite; Nunes, Ana Luiza; Rademaker, Alexandre. Extracting valences from a dependency treebank for populating the verb lexicon of a Portuguese HPSG grammar. Proceedings of the 15th International Conference on Computational Processing of Portuguese (PROPOR 2022), March 21st to March 23rd, Fortaleza, Brazil. Cham, Switzerland: Springer, 2022. Forthcoming.
"""

import sys, os, re, pickle
USER=os.path.expanduser("~")
#sys.path.append(os.path.join(USER, "scripts"))
import PorGramEntries, ValenceExtractor
from delphin import tdl
from valences import *
DIR1=os.path.join(USER, "hpsg/por")
DIR2=os.path.join(USER, "hpsg/por/tmp")
# list of the names of the files defining the mapping from UD valence frames to PorGram types 
FILENAMES=['prep-obj-2ng-arg-verbs','clausal-verbs', 'intrans-trans', 'ditrans']
# destination file of the created entries
OUTFILE="bosque-entries.tdl"
SCRIPTFILE=os.path.join(USER, DIR1,'lkb/my-script')

def build_mapping(filename):
    return dict([re.split(r"\s+",line.strip()) for line in open(os.path.join(USER,DIR1,filename),"r").readlines() if line.strip() != ""])

#MAPPING=build_mapping()

# table mapping lexical identifiers to types in the existing TDL lexicon files 
INFILE=os.path.join(USER, DIR2,"verbtypes142256.txt")

def build_old_lexicon(lkb_script=SCRIPTFILE):
    filenames=extract_filenames(lkb_script)
    return extract_lexicons(filenames)

def extract_filenames(lkb_script=SCRIPTFILE):
    """This function extracts the names of the lexicon files in a LKB load script.
"""
    s=open(lkb_script,'r').read()
    regex1=re.compile(r"@start.*@end",re.DOTALL)
    regex2=re.compile(r"[-a-z0-9]+\.tdl",re.DOTALL)
    return regex2.findall(regex1.findall(s)[0])

def convert_lexicon(lexicon):
    new_lexicon={}
    for ident,td in lexicon.items():
        verbtype=str(td.supertypes[0])
        lemma=str(td.features()[0][1].values()[0])
        verbtype_set=new_lexicon.get(lemma)
        if verbtype_set:
            new_lexicon[lemma].add(verbtype)
        else:
            new_lexicon[lemma]={verbtype}
    return new_lexicon

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
 
def build_entry(lemma,verb_type,index,pos="v"):
    return f"""{lemma}_{pos}{index} := {verb_type} &
  [ STEM < "{lemma}" >,
    SYNSEM.LKEYS.KEYREL.PRED "_{lemma}_{pos}_{index}_rel" ].\n"""
  
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
    first_lexicon=lexicons[0]
    other_lexicons=lexicons[1:]
    updated_lexicon={}
    updated_lexicon.update(first_lexicon)
    for lexicon in other_lexicons:
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
			
def write_lexicon(new_lexicon,path_to_dir=DIR1, outfile=OUTFILE,old_lexicon=None,pos="v"):
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
                    print(build_entry(lemma,verbtype,index,pos),file=outfile)
                    index+=1
        else:
            for verbtype in new_verbtypes:
                print(build_entry(lemma,verbtype,index,pos),file=outfile)
                index+=1
    outfile.close()
 
def build_lexicons(filenames=FILENAMES):
    new_lexicons=[]
    for filename in filenames:
        infile=f"{filename}.txt"
        #outfile=f"{filename}.pkl"
        mapping=build_mapping(infile)
        new_lexicon=build_lexicon(mapping)
        #save_lexicon(new_lexicon,outfile)
        new_lexicons.append(new_lexicon)
    return new_lexicons

def extract_lexicons(filenames):
    """Returns the entries of multiples TDL lexicon files in form of a dictionary. 

    Parameters:
    argument1 (list): List of strings representing the name of TDL lexicon files.

    Returns:
    dict: A dictionary mapping each lemma (str) in the lexicon files to its set of types (str).
    """
    lexicons=[]
    for filename in filenames:
        lexicons.append(convert_lexicon(ValenceExtractor.parse_tdl(filename)))
    return join_lexicons(*lexicons)
        
def main(old_lexicon, outfile=OUTFILE, pos="v", filenames=FILENAMES):
    lexicons_list=build_lexicons(filenames)
    new_lexicon=join_lexicons(*lexicons_list)
    collapse_dat_goa(new_lexicon)
    write_lexicon(new_lexicon,outfile=outfile,old_lexicon=old_lexicon,pos=pos)
		

if __name__ == "__main__":
    if len(sys.argv) > 1:
        if sys.argv[1].endswith(".pkl"):
            old_lexicon=load_lexicon(sys.argv[1])
            main(old_lexicon,outfile=sys.argv[2], pos=sys.argv[3], filenames=sys.argv[4:])
        else:
            old_lexicon=build_old_lexicon()
            main(old_lexicon,outfile=sys.argv[1], pos=sys.argv[2], filenames=sys.argv[3:])
    else:
        from PorGramEntries import MakeDictionary, ExtractEntries
        main(old_lexicon=MakeDictionary(ExtractEntries(INFILE)))
