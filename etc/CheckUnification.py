# -*- coding: utf-8 -*-

from __future__ import print_function
from nltk import FeatStruct as fs
import re
import json
import conllu
from io import open
from conllu import parse_incr
import sys
import os

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

with open('morphobr_to_bosque.json') as f:
    morphobr_to_bosque = json.load(f)


def check(token_fst,entries):
    """Return the list of conflicting attribute-value pairs between a
    treebank feature structure for a token and a list of dictionary
    entries for this token. If the structures unify with at least one
    entry, return the empty list. Otherwise return the inconsistencies
    collected by means of the function find_error.

    """
    errors=[]
    for entry in entries:
        if entry.unify(token_fst):
            return []
        else:
            errors.append(entry)
    return [find_error(error,token_fst) for error in errors]


def token_to_fst(token,lemma,cat,feats):
    d =[('Form',token),('Lemma',lemma),('Cat',cat)]
    if type(feats) is dict:
        for feat in feats.keys():
            d.append((feat,feats[feat]))
    return fs(dict(d))

def entry_to_fst(entry="simples simples+A+F+PL"):
    ls = re.split(r"[ \+\t]",entry)
    d = [('Form',ls[0]),('Lemma',ls[1])]
    ps = re.split(r"[\.]",ls[2].strip())
    if len(ps) > 1:
        d.append(('Cat','VERB'))
        e = [('Lemma',ps[1])]
        for p in ps[2:]:
            ms = morphobr_to_bosque.get(p.strip())
            for m in ms:
                f = re.split(r"[\=]",m)
                e.append((f[0],f[1]))
        d.append(('Pron',fs(dict(e))))
        for l in ls[3:]:
            ms = morphobr_to_bosque.get(l.strip())
            for m in ms:
                f = re.split(r"[\=]",m)
                d.append((f[0],f[1]))
    else:
        for l in ls[2:]:
            ms = morphobr_to_bosque.get(l.strip())
            for m in ms:
                f = re.split(r"[\=]",m)
                d.append((f[0],f[1]))
    return (ls[0], fs(dict(d)))

def tokens_to_fst(token,lemma1,cat,feats1,lemma2,feats2):
    d = [('Form',token),('Lemma',lemma1),('Cat',cat)]
    e = [('Lemma',lemma2)]
    if type(feats1) is dict:
        for feat in feats1.keys():
            d.append((feat,feats1[feat]))
    if type(feats2) is dict:
        for feat in feats2.keys():
            e.append((feat,feats2[feat]))
    d.append(('Pron',fs(dict(e))))
    return fs(dict(d))

def find_error(fs1,fs2):
    attributes=set(fs1.keys()).union(fs2.keys())
    errors=[]
    for k in attributes:
        v1=fs1.get(k)
        v2=fs2.get(k)
        if v1 and v2 and v1 != v2:
            errors.append([k,v1,v2])
    return errors


def extract_entries(d, infile):
    with open(infile) as f:
        for line in f.readlines():
            form, fs = entry_to_fst(line)
            if d.get(form):
                d[form].append(fs)
            else:
                d[form] = [fs]


def readMorpho(path):
    adict = {}
    dirs = ["nouns","adjectives","adverbs","verbs"]
    for d in dirs:
        eprint("Loading %s." % d)
        for root,_, files in os.walk(os.path.join(path,d), topdown=False):
            for file in files:
                extract_entries(adict, os.path.join(root,file))
    return adict


def errors2string(errors):
    return " | ".join([ " § ".join([ "%s:%s≠%s" %(e[0],e[1],e[2]) for e in ue]) for ue in errors])

def proc1(morpho, content):
    for sent in conllu.parse_incr(content):
        msg = "\n%s: %s " % (sent.metadata.get('sent_id'), sent.metadata.get('text'))
        flag = False
        for token in sent:
            if token["upos"] in ["ADJ","ADV","NOUN","VERB"]:
                tfs = token_to_fst((token["form"]).lower(),token["lemma"],token["upos"],token["feats"])
                candidates = morpho.get((token["form"]).lower())
                if candidates:
                    errors = check(tfs, candidates)
                    if len(errors)>0:
                        msg = msg + " ['%s' %s] " %(token,errors2string(errors))
                        flag = True
                else:
                    msg = msg + " ['%s' NF] " % token
                    flag = True
        if flag:
            print(msg)
                    

def proc2(morpho, content):
    for sent in conllu.parse_incr(content):
        sid = sent.metadata.get('sent_id')
        for token in sent:
            if token["upos"] in ["ADJ","ADV","NOUN","VERB"]:
                tid = "%s" % token["id"]
                tfs = token_to_fst((token["form"]).lower(),token["lemma"],token["upos"],token["feats"])
                candidates = morpho.get((token["form"]).lower())
                if candidates:
                    errors = check(tfs, candidates)
                    if len(errors)>0:
                        for ue in errors:
                            for e in ue:
                                print("\t".join([sid,tid,token["form"],e[0],e[1],e[2]]))
                else:
                    print("\t".join([sid,tid,token["form"],'NF','_','_']))

def procVerbs(morpho,content):
    for sent in conllu.parse_incr(content):
        msg = "\n%s: %s " % (sent.metadata.get('sent_id'), sent.metadata.get('text'))
        flag = False
        i = 0
        while i < len(sent):
            if i+1 == len(sent) and sent[i]["upos"] in ["VERB","AUX"]:
                tfs=token_to_fst(sent[i]["form"].lower(),sent[i]["lemma"],sent[i]["upos"],sent[i]["feats"])
                candidates = morpho.get((sent[i]["form"]).lower())
                if candidates:
                    errors = check(tfs, candidates)
                    if len(errors)>0:
                        msg = msg + " ['%s' %s] " %(sent[i],errors2string(errors))
                        flag = True
                else:
                    msg = msg + " ['%s' NF] " % sent[i]
                    flag = True
                i +=1
            elif sent[i]["form"] != "--" and len(re.split(r"[\-]",sent[i]["form"]))>1 and sent[i+1]["upos"] == "VERB" and sent[i+2]["upos"] == "PRON":
                tfs=tokens_to_fst(sent[i+1]["form"].lower(),sent[i+1]["lemma"],sent[i+1]["upos"],
                                 sent[i+1]["feats"],sent[i+2]["lemma"],sent[i+2]["feats"])
                candidates = morpho.get((sent[i]["form"]).lower())
                if candidates:
                    errors = check(tfs, candidates)
                    if len(errors)>0:
                        msg = msg + " ['%s' %s] " %(sent[i],errors2string(errors))
                        flag = True
                else:
                    msg = msg + " ['%s' NF] " % sent[i]
                    flag = True
                i += 3       
                
            elif len(sent) and sent[i]["upos"] in ["VERB","AUX"]:
                tfs=token_to_fst(sent[i]["form"].lower(),sent[i]["lemma"],sent[i]["upos"],sent[i]["feats"])
                candidates = morpho.get((sent[i]["form"]).lower())
                if candidates:
                    errors = check(tfs, candidates)
                    if len(errors)>0:
                        msg = msg + " ['%s' %s] " %(sent[i],errors2string(errors))
                        flag = True
                else:
                    msg = msg + " ['%s' NF] " % sent[i]
                    flag = True
                i +=1


def execute():
    morpho = readMorpho(sys.argv[1])
    for path in sys.argv[2:]:
        with open(path) as content:
            proc1(morpho, content)

def usage():
    print("\nUsage:\n\tpython CheckUnification.py path-morphobr conllu1 conllu2 ... \n\n")


if __name__ == "__main__":
    if len(sys.argv) < 3:
        usage()
    else:
        execute()

