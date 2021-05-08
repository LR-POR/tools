#! /usr/bin/env python2.7
# -*- coding: utf-8 -*-

from nltk import FeatStruct as fs
"""
This module shows how to use unification to detect errors in a lexical resource or treebank,
comparing the two resources against one another.

Sketch of the algorithm:

for sentence in treebank:
    for word in sentence:
        token=sentence.get(word)
        entries=morphobr.get(word)
        for entry in entries:
            e=convert(entry,morpho)
            t=convert(entry,bosque)
            check_unification(t, e)
            
Example sentence:            
pt_bosque-ud-train.conllu-# text = «É uma obra que fala de fé e eu espero que possibilite ao público uma compreensão direta do gospel, uma música de palavras simples e profundas.»
pt_bosque-ud-train.conllu-# sent_id = CF733-3
pt_bosque-ud-train.conllu-# source = CETENFolha n=733 cad=Ilustrada sec=nd sem=94b
pt_bosque-ud-train.conllu-# id = 3072

pt_bosque-ud-train.conllu-27	palavras	palavra	NOUN	_	Gender=Fem|Number=Plur	25	nmod	_	_
pt_bosque-ud-train.conllu:28	simples	simples	ADJ	_	Gender=Masc|Number=Plur	27	amod	_	_

"""
"underspecified entry in MorphoBr"
ENTRY=fs("[lemma='simples',form='simples', cat='A']")

"(corrected) analysis of Bosque"
TOKEN=fs("[lemma='simples',form='simples', cat='A',gend='f', num='pl']")

"bogus incorrect analysis"
ERROR=fs("[lemma='simpls',form='simpls', cat='A',gend='f', num='pl']")

def bosque_to_fst(token="simples simples ADJ Gender=Fem|Number=Plur"):
    """code to be implemented"""
    return TOKEN

def morphobr_to_fst(token="simples  simples+A+F+PL"):
    """code to be implemented"""
    return ENTRY

def convert(token,resource):
    if resource == "bosque":
       bosque_to_fst(token)
    if resource == "morpho":
       morphobr_to_fst(token)
       
def find_error(fs1,fs2):
	attributes=set(fs1.iterkeys()).union(fs2.iterkeys())
	for k in attributes:
		v1=fs1.get(k)
		v2=fs2.get(k)
		if v1 and v2 and not v1 == v2:
			print "values '%s' and '%s' of '%s' don't match" % (v1, v2,k)
		
def check_unification(fs1,fs2):
    msg="feature structures%s unify"
    if fs1.unify(fs2):
         print msg % ""
    else:
        print msg % " don't"
        find_error(fs1,fs2)
    

def demo():
    print "%s\n\n%s\n" % (TOKEN,ENTRY)   
    check_unification(TOKEN, ENTRY)
    print "\n%s\n\n%s\n" % (ERROR,ENTRY)
    check_unification(ERROR, ENTRY)

        

