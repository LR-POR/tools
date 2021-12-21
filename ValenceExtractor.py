#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author: Leonel Figueiredo de Alencar Date Dec. 21, 2021

import os,sys,re
import conllu, pickle, numpy
from valences import *
USER=os.path.expanduser("~")
VALENCES = joblib.load(os.path.join(USER,"tools/etc/valence_script/valences_dict.joblib"))
FRAMES=list(VALENCES.keys())

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
    for verb in VALENCES[valence_category]:
        if verb.lemma == lemma:
            for valence in verb.valences:
                if valence.valence_category == valence_category:
                    #print(valence.example)
                    #print(valence.example,file=tmp)
                    examples.append(valence.example)
    #tmp.close()
    return examples

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


def extract_prepositions(frames=FRAMES,pat=r"VERB:act,(nsubj,)?expl,obj:([a-z]+)"):
	preps=set()
	for f in frames:
		s=re.search(pat,f)
		if s:
			preps.add(s.groups()[1])
	return preps


def extract_frames():
    return list(VALENCES.keys())

def extract_examples_of_frames(frames=FRAMES,pattern=r"ccomp:(a|com|para)\+(Ind|Sub)"):
    for frame in frames:
        if re.search(pattern,frame):
            verbs=extract_verbs([frame])
            lemmas=[verb.lemma for verb in verbs]
            print(f"{frame}\t({', '.join(lemmas)})")
            for lemma in lemmas:
                print(lemma,": ","\n".join(extract_example(frame,lemma)),sep= " ")
            print()


"""def main():
    results(sys.argv[1])"""
    
if __name__ == '__main__':
	main()
