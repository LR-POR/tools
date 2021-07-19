import delphin.tdl as tdl
import json

letterSet = []
rules = []

for event, obj, lineno in tdl.iterparse('PorGram/my-irules.tdl'):
    if event == 'LetterSet':
        letterSet.append({'var':obj.var,'characters':obj.characters}) 
    elif event == 'LexicalRuleDefinition':
        rules.append({'identifier': obj.identifier,'affix_type':obj.affix_type,'patterns':obj.patterns})


with open('irules.json', 'w') as f:
    json.dump({'letterSet':letterSet,'rules':rules}, f)