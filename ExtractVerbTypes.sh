#! /bin/bash
# my-lexicon.tdl lexicon.tdl
# Author: Leonel Figueiredo de Alencar Date Dec. 21, 2021

grep -Ehv "^;" "$@"  | awk '$2 ~ /:=/ && $3 ~ /(verb\-lex|aux)/ {print $1, $3}'  | uniq | sort > tmp/verbtypes$$.txt

awk '{print $2}' tmp/verbtypes$$.txt | sort | uniq -c | sort -nr > tmp/freqdist$$.txt

awk '{sum+=$1;} END{printf "total number of instances: %d\ntotal number of types: %d\n", sum, NR}' tmp/freqdist$$.txt

# awk '{print $1}' freqdist$$.txt | sort | uniq -c | sort -nr

cd tmp
PorGramEntries.py verbtypes$$.txt




