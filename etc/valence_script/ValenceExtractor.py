#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author: Leonel Figueiredo de Alencar
# Sample generation of lexical entries from TDL file based on 
# https://github.com/LR-POR/tools/commit/4f3b38843ed4a2626f3c75d6043542deb3daf74a
# Date: Dec. 21, 2021

import os,sys,re
import conllu, pickle, numpy
from delphin import tdl
import random
from valences import *
from WriteVerbEntries import from_frames_to_types
USER=os.path.expanduser("~")
INSTALL="tools/etc"
#sys.path.append(os.path.join(USER, INSTALL))
FILENAME="bosque-master-20211210.pickle"
PATH_TO_DICTIONARY=os.path.join(USER,INSTALL,FILENAME)
VALENCES = joblib.load(PATH_TO_DICTIONARY)
FRAMES=list(VALENCES.keys())
MAPPING=from_frames_to_types()

def insert_docstring(td,mapping):
    examples=get_examples_of_verbtype(str(td.supertypes[0]),str(td.features()[0][1].values()[0]), mapping=mapping)
    td.docstring=get_shortest_example(examples)
    
def parse_tdl(infile):
    lexicon={}
    for event, td, lineno in tdl.iterparse(infile):
        if event == 'TypeDefinition':
            lexicon[td.identifier] = td
    return lexicon

def format_lexicon(lexicon,outfile):
    """Formats and writes lexical entries to a file. 

    Parameters:
    argument1 (dict): Dictionary mapping lexicon identifiers (strings) to TypeDefinition objects.
    argument2 (str): Name of output file.

    Returns:
    set: A set of strings representing the prepositions in the list frames.
    """
    outfile=open(outfile,'w')
    for lexid,td in lexicon.items():
        print(tdl.format(td),"\n",file=outfile)
    outfile.close()
        
def insert_examples(infile,outfile,sample=0,mapping=MAPPING):
    """This function makes a new version of a TDL file with lexical entries automatically created from UD_Portuguese-Bosque, inserting as a docstring the corresponding shortest example in the treebank. If the sample parameter is greater than 0, a random sample with the given number of entries is created.    
    """
    outfile=open(outfile,'w')
    lex=parse_tdl(infile)
    #newlex={}
    #for event, td, lineno in tdl.iterparse(infile):
    #   if event == 'TypeDefinition':
    #      lex[td.identifier] = td
    if sample:
        for ident in random.sample(lex.keys(), sample):
            td =lex[ident]
            insert_docstring(td,mapping)
            #newlex[ident]=td
            print(tdl.format(td),"\n",file=outfile)
    else:
        for ident,td in lex.items():
            insert_docstring(td,mapping)
            print(tdl.format(td),"\n",file=outfile)

    outfile.close()
            

def compute_stats(frames=FRAMES):
    frame_lens=[(frame,len(frame.split(","))) for frame in frames]
    max_len=max(flen for f,flen in frame_lens)
    dic={}
    for n in range(1,max_len+1):
        for f,flen in frame_lens:
            if flen == n:
                if dic.get(flen):
                    dic[n]+=1
                else:
                    dic[n]=1
    for k,v in dic.items():
        print(k,v)

		
def parse_frame(frame):
    match=re.match(r"<(.+)>",frame)
    if match:
        return match.groups()[0].split(",")
    else:
        return []


def extract_syntactic_functions(syntactic_function,frames):
    freqdist={}
    for frame in frames:
        items=parse_frame(frame)
        for item in items:
            if item.startswith(syntactic_function):
                if freqdist.get(item):
                    freqdist[item]+=1
                else:
                    freqdist[item]=1
    return freqdist

def extract_example(valence_category,lemma):
    examples=[]
    #tmp=open('tmp.tmp','w')
    verbs=VALENCES.get(valence_category)
    if verbs:
        for verb in verbs:
            if verb.lemma == lemma:
                for valence in verb.valences:
                    if valence.valence_category == valence_category:
                        #print(valence.example)
                        #print(valence.example,file=tmp)
                        examples.append(valence.example)
    #tmp.close()
    return examples

def get_examples_of_verbtype(verbtype,verb,expand=True,dative=True,mapping=MAPPING):
    examples=[]
    for frame,basictype in mapping.items():
        frames=[]
        if basictype==verbtype:
            if expand:
                frames=expand_valence(frame,dative)
            else:
                frames.append(frame)
        elif "-rec-" in verbtype:
            if expand:
                frames=expand_valence('<VERB:act,nsubj,iobj:a,obj>',dative)
                frames.extend(expand_valence('<VERB:act,nsubj,iobj:para,obj>',dative))
            else:
                frames.extend(['<VERB:act,nsubj,iobj:a,obj>','<VERB:act,nsubj,iobj:para,obj>'])
        for frame in frames:
                examples.extend(extract_example(frame,verb))
    return examples

def pprint_examples(examples):
    print("\n".join(examples))

def get_shortest_example(examples):
    shortest=""
    c=len(examples)
    if c > 1:
        shortest=examples[0]
        for example in examples[1:]:
            if len(example) < len(shortest):
                shortest=example
    elif c == 1:
        shortest=examples[0]
    return shortest

def expand_valence(frame,dative=True):
	frames1=[]
	frames2=[]
	frames1.append(frame)
	if re.search(r",obj[^:]",frame):
		frames1.append(frame.replace("VERB:act","VERB:pass").replace(",obj",""))
	for f in frames1:
		frames2.append(f.replace(",nsubj",""))
	frames1.extend(frames2)
	if dative:
		frames3=[]
		for f in frames1:
			if re.search(r",iobj:a[,>]",f):
				frames3.append(f.replace(",iobj:a",",iobj"))
		frames1.extend(frames3)
	return frames1

def extract_verbs(frames):
	itens=set()
	for e in frames:
		verbs=VALENCES.get(e)
		if verbs:
			itens.update(verbs)
	return itens


def extract_prepositions(pat=r"(,)(i?obj:)(\w+)([,>])",frames=FRAMES):
    """Returns the set of prepositions from a list of valence frames. 

    Parameters:
    argument1 (str): Regular expression pattern for matching prepositions.
    argument1 (list): List of valence frames.

    Returns:
    set: A set of strings representing the prepositions in the list frames.
   """
    frames_with_preps=[frame for frame in frames if "obj:" in frame]
    preps=set()
    for frame in frames_with_preps:
        s=re.search(pat,frame)
        if s:
            preps.add(s.groups()[2])
    return preps


def extract_frames():
    return list(VALENCES.keys())

def extract_examples_of_frames(pattern=r"ccomp:(a|com|para)\+(Ind|Sub)",outfile=sys.stdout,frames=FRAMES):
    for frame in frames:
        if re.search(pattern,frame):
            verbs=extract_verbs([frame])
            lemmas=[verb.lemma for verb in verbs]
            print(f"{frame}\t({', '.join(lemmas)})",file=outfile)
            for lemma in lemmas:
                print(lemma,": ","\n".join(extract_example(frame,lemma)),sep= " ",file=outfile)
            print("\n",file=outfile)
    if outfile != sys.stdout:
        outfile.close()


"""def main():
    results(sys.argv[1])"""
    
if __name__ == '__main__':
	main()
