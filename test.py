#! /usr/bin/env python2.7
# -*- coding: utf-8 -*-

from nltk import FeatStruct as fs
import re
import json
import conllu

with open('/home/ana/dhbb/check-tools/morphobr_to_bosque.json') as f:
    morphobr_to_bosque = json.load(f)

def bosque_to_fst(token,lemma,cat,feats):
    d =[('Form',token),('Lemma',lemma),('Cat',cat)]
    if type(feats) is dict:
        for feat in feats.keys():
            d.append((feat,feats[feat]))
    return fs(dict(d))

def morphobr_to_fst(token="simples simples+A+F+PL"):
    ls = re.split(r"[ \+]",token)
    d = [('Form',ls[0]),('Lemma',ls[1])]
    for l in ls[2:]:
        ms = morphobr_to_bosque.get(l)
        for m in ms:
            f = re.split(r"[\=]",m)
            d.append((f[0],f[1])) 
    return fs(dict(d))

file = open("/home/ana/dhbb/dhbb-nlp/udp-mini/161.conllu", "r", encoding="utf-8")
data_file = file.read()
for sent in conllu.parse(data_file):
    for token in sent:
        print(bosque_to_fst(token["form"],token["lemma"],token["upos"],token["feats"]))