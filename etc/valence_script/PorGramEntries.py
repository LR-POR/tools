#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author: Leonel Figueiredo de Alencar Date Dec. 21, 2021

import sys

def ExtractEntries(infile):
	return [entry.strip().split() for entry in open(infile,encoding="utf-8").readlines() if entry.strip() != ""]

def MakeDictionary(entries):
	dic={}
	for ident,lextype in entries:
		lemma=ident.split("_")[0]
		if dic.get(lemma):
			dic[lemma].append(lextype)
		else:
			dic[lemma]=[lextype]
	return dic
    

def getMaxLength(dic):
    """get the maximum list length of the values of a dictionary mapping lemmas to lists
    of lexical types
    """
    m=1
    for k in dic.keys():
        c=len(dic[k])
        if c>m:
            m=c
    return m

def sortEntries(dic):
    sortedentries=[]
    for n in range(1,getMaxLength(dic)+1):
        for k in dic.keys():
            if len(dic[k]) == n:
                sortedentries.append((n,k,dic[k]))
    return sortedentries

def main(infile="/home/leonel/hpsg/por/verb-types.txt"):
    outfile=open(f"/home/leonel/hpsg/por/tmp/{infile.split('.')[0]}.freqs","w")
    entries=ExtractEntries(infile)
    dic=MakeDictionary(entries)
    sortedentries=sortEntries(dic)
    for num,lemma,types in sortedentries:
        print(num,lemma," ".join(types),file=outfile)
    outfile.close()
    
if __name__ == "__main__":
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        main()


