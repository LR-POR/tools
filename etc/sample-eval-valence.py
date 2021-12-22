
from delphin import tdl
import random

lex = {}
for event, obj, lineno in tdl.iterparse('new-lexicon-rec.tdl'):
    if event == 'TypeDefinition':
        lex[obj.identifier] = obj


ks = random.sample(lex.keys(), 50)
for k in ks:
    print(tdl.format(lex[k]))
    print("")
    
    
