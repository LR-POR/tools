#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author: Leonel Figueiredo de Alencar Date Dec. 21, 2021

import sys, os, re, pickle
import PorGramEntries, ValenceExtractor
USER=os.path.expanduser("~")
DIR1=os.path.join(USER, "hpsg/por")
DIR2=os.path.join(USER, "hpsg/por/tmp")

MAPPING=dict([re.split(r"\s+",line.strip()) for line in open(os.path.join(USER,DIR1,"mapping.txt"),"r").readlines() if line.strip() != ""])

INFILE=os.path.join(USER, DIR2,"verbtypes142256.txt")

LEXICON=PorGramEntries.MakeDictionary(PorGramEntries.ExtractEntries(INFILE))

def save_lexicon(lexicon,filename):
    outfile = open(filename,'wb')
    pickle.dump(lexicon,outfile)
    outfile.close()
    
def load_lexicon(lexicon,filename):
    outfile = open(filename,'rb')
    lexicon=pickle.load(lexicon,outfile)
    outfile.close()
    return lexicon

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
            new_lexicon[verb.lemma]=[verb_type]
    else:
        new_lexicon[verb.lemma].append(verb_type)
  
def build_lexicon(infile="/home/leonel/hpsg/por/tmp/verbtypes142256.txt"):
    frames=list(MAPPING.keys())
    new_lexicon={}
    for frame in frames:
        framelist=ValenceExtractor.expand_valence(frame,False)
        verbs=ValenceExtractor.extract_verbs(framelist)
        for verb in verbs:
            #entry=build_entry(verb,MAPPING[frame])
            #if entry:
                #print(entry)
            expand_lexicon(verb,MAPPING[frame],new_lexicon)
    return new_lexicon
    #outfile=open(f"/home/leonel/hpsg/por/bosque-entries.tdl","w")
    #for num,lemma,types in sortedentries:
     #   print(num,lemma," ".join(types),file=outfile)
    #outfile.close()
 
def write_lexicon(new_lexicon,path_to_dir=DIR1, outfile="bosque-entries.tdl",old_lexicon=LEXICON):
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
    
if __name__ == "__main__":
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        main()
