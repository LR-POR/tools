#! /usr/bin/env python2.7
# -*- coding: utf-8 -*-

from nltk import FeatStruct as fs
"""
This module shows how to use unification to detect errors in a lexical resource or treebank,
comparing the two resources against one another.

Sketch of the algorithm:

TODO: create class UD_BosqueTreebank with method tagged_sents etc.
(e.g. subclass of nltk.corpus.CorpusReader)
TODO: create Python dictionary from MorphoBr, e.g.
morpho={'baratas':["barato+A+F+PL","barata+N+F+PL","baratar+V+PRS+2+SG"], ...}


treebank=UD_BosqueTreebank("pt_bosque-ud-*.conllu")
sentences=treebank.tagged_sents()
for sentence in sentences:
    for word,tag in sentence:
        token_fst=convert(tag,bosque) # e.g. convert("simples+ADJ+Gender=Fem|Number=Plur",bosque)
        entries=morphobr.get(word)
        if entries:
            errors=check(token_fst,[convert(e) for e in entries])
            if errors:
                pprint(errors)
        else:
            print "not found in dict"
            
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
ENTRY2=fs("[lemma='simples',form='simples', cat='N']")
ENTRY3=fs("[lemma='simpls',form='simples', cat='N']")
ENTRY4=fs("[lemma='simpls',form='simples', cat='X']")
ENTRY5=fs("[lemma='simples',form='simpls', cat='Y']")

ENTRIES=[ENTRY, ENTRY2, ENTRY3, ENTRY4, ENTRY5]

"(corrected) analysis of Bosque"
TOKEN=fs("[lemma='simples',form='simples', cat='A',gend='f', num='pl']")

"bogus incorrect analysis"
ERROR=fs("[lemma='simpls',form='simpls', cat='A',gend='f', num='pl']")

def check(token_fst,entries):
    """Return the list of conflicting attribute-value pairs between a treebank
    feature structure for a token and a list of dictionary entries for this token.
    If the structures unify, return the empty list. Otherwise return the inconsistencies
    collected by means of the function find_error."""

    errors=[]
    for entry in entries:
	if entry.unify(token_fst):
            return []
	else:
	    errors.append(entry)
    return [find_error(error,token_fst) for error in errors]
	    

def bosque_to_fst(token="simples simples ADJ Gender=Fem|Number=Plur"):
    """TODO code to be implemented"""
    return TOKEN

def morphobr_to_fst(token="simples  simples+A+F+PL"):
    """TODO code to be implemented"""
    return ENTRY

def convert(token,resource):
    if resource == "bosque":
       bosque_to_fst(token)
    if resource == "morpho":
       morphobr_to_fst(token)
       
def find_error(fs1,fs2):
	attributes=set(fs1.iterkeys()).union(fs2.iterkeys())
	errors=[]
	for k in attributes:
		v1=fs1.get(k)
		v2=fs2.get(k)
		if v1 and v2 and not v1 == v2:
                    errors.append((k,v1,v2))
        return errors

def pprint_errors(errors):
    for list_of_errors in errors:
        for error in list_of_errors:
            print "attribute '%s': values '%s' and '%s' don't match" % error
		
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
    print "\n%s\n" % ("in case of unification nothing is printed")
    check(TOKEN, ENTRIES)
    print "\n%s\n" % ("showing why unification failed")
    ENTRIES.pop(0)
    check(TOKEN, ENTRIES)

        

