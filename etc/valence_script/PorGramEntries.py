#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Author: Leonel Figueiredo de Alencar Date Dec. 21, 2021

import os,sys
USER=os.path.expanduser("~")
PORGRAM=os.path.join(USER,"hpsg/por")

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

def perc(part,total):
	return (100/total)*part

def computeStats(dic):
    m=getMaxLength(dic)
    entries=sortEntries(dic)
    lemmas=len(entries)
    print("types\tlemmas\tperc")
    for num in range(1,m+1):
        total=0
        for entry in entries:
            if entry[0]==num:
                total+=1
        print(f"{num}\t{total}\t{perc(total,lemmas):.2f}")
    
def main(infile=os.path.join(USER,PORGRAM,"tmp/verbtypes142256.txt")):
    path_to_outfile=os.path.join(USER,PORGRAM,"tmp",f"{infile.split('.')[0]}.freqs")
    outfile=open(path_to_outfile,"w")
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


