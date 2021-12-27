from conllu import parse
import joblib


DEPRELS = ['nsubj','obj','iobj','xcomp','ccomp','csubj','expl','nsubj:pass','aux:pass']

## Funçoes auxiliares

def binary_search(x,l):
    """ Esse algorítmo é o algorítmo de busca binária, mas ele retorna
    qual o índice o qual devo colocar o elemento para que a lista
    permaneça ordenada.

    Input: elemento x e lista l
    Output: Índice em que o elemento deve ser inserido para manter a ordenação da lista
    """
    lo = 0 # Cota inferior inicial (Lower bound)
    up = len(l) # Cota superior inicial (Upper bound)
    while lo < up:
        mid = int((lo+up)/2) #Ponto Médio 
        if l[mid] < x:
            lo = mid + 1
        else:
            up = mid
    return up

def convert_token_to_relation(sentence,token):
    """ Converte um objeto do tipo token da biblioteca conllu em um objeto
    do tipo Relation criado aqui
    """
    return Relation(sentence,token)


class Relation:
    """ Objeto do tipo relation, tem basicamente as informações do token e
    também de seus filhos que nos interessam (case e mark)
    """
    def __init__(self,sentence,token):
        self.token = token['form']
        self.lemma = token['lemma']
        self.deprel = token['deprel']
        self.upos = token['upos']
        self.feats=token['feats']
        self.relation = []
        son_tokens = get_son_tokens(sentence,token)
        if son_tokens != []:
            for son_token in son_tokens:
                if son_token['deprel'] in ('case','mark'):
                    self.relation.append(convert_token_to_relation(sentence,son_token))
  
        
    def __str__(self):
        return f'[{str(self.lemma)},{str(self.deprel)},{str(self.upos)}]'
    def __repr__(self):
        return f'[{str(self.lemma)},{str(self.deprel)},{str(self.upos)}]'
    

class Valence:
    """ Objeto do tipo valencia, que guarda as infos específicas de um
    determinado verbo em uma determinada sentença, como seu xcomp,
    ccomp, obj etc. caso haja.  Possui um método print para imprimir
    as informaçoes da valência do verbo.
    """
    def __init__(self,
                 token,
                 lemma=None,
                 feats=None, 
                 xcomp=None,
                 ccomp=None,
                 obj=None,
                 iobj=None,
                 expl=None,
                 nsubj=None,
                 csubj=None,
                 nsubj_pass=None,
                 aux_pass = None,
                 example=None,
                 rel_set = None):
        
        self.token = token
        self.lemma = lemma
        self.feats = feats
        self.xcomp = xcomp
        self.ccomp = ccomp
        self.obj = obj
        self.iobj = iobj
        self.expl = expl
        self.nsubj = nsubj
        self.csubj = csubj
        self.nsubj_pass = nsubj_pass
        self.aux_pass = aux_pass
        self.example = example
        self.rel_set = []
        rel_string = ''
        if self.obj is not None:
            obj = 'obj'
            obj_complement = ''
            if len(self.obj.keys()) > 0:
                val = list(self.obj.keys())[0]
                rel = self.obj[val]
                if len(rel.relation) > 0:
                    for relation in rel.relation:
                        if relation.upos == 'ADP':
                            obj_complement += f':{relation.lemma}'
                            break
            obj += obj_complement
            self.rel_set.append(obj)
        if self.iobj is not None:
            iobj = 'iobj'
            iobj_complement = ''
            if len(self.iobj.keys()) > 0:
                val = list(self.iobj.keys())[0]
                rel = self.iobj[val]
                if len(rel.relation) > 0:
                    for relation in rel.relation:
                        if relation.upos == 'ADP':
                            iobj_complement += f':{relation.lemma}'
            iobj += iobj_complement            
            self.rel_set.append(iobj)
        if self.ccomp is not None:
            ccomp = 'ccomp'
            ccomp_complement = ''
            if len(self.ccomp.keys()) > 0:
                val = list(self.ccomp.keys())[0]
                rel = self.ccomp[val]
                rel.relation.sort(key = lambda x: x.lemma)
                for relation in rel.relation:
                    if relation.upos == 'SCONJ':
                        ccomp_complement += f'+{relation.lemma}'
                        break
                if val == 'VERB':
                    if 'Mood' in rel.feats.keys():
                        ccomp_complement += f"+{rel.feats['Mood']}"
            if len(ccomp_complement) > 0 and ccomp_complement[0] == '+':
                ccomp_complement = ':' + ccomp_complement[1:]
            ccomp+=ccomp_complement
            self.rel_set.append(ccomp)
        if self.xcomp is not None:
            xcomp = 'xcomp'
            xcomp_complement = ''
            if len(self.xcomp.keys()) > 0:
                val = list(self.xcomp.keys())[0]
                rel = self.xcomp[val]
                rel.relation.sort(key = lambda x: x.lemma)
                for relation in rel.relation:
                    if relation.upos == 'SCONJ':
                        xcomp_complement += f'+{relation.lemma}'
                if val == 'VERB':
                    if 'VerbForm' in rel.feats.keys():
                        xcomp_complement += f'+{rel.feats["VerbForm"]}'
            if len(xcomp_complement) > 0 and xcomp_complement[0] == '+':
                xcomp_complement = ':' + xcomp_complement[1:]
            xcomp += xcomp_complement
            self.rel_set.append(xcomp)
        if self.csubj is not None:
            self.rel_set.append('csubj')
        if self.expl is not None:
            self.rel_set.append('expl')
            
        if self.aux_pass is not None or self.nsubj_pass is not None or (self.feats is not None and 'Voice' in self.feats.keys() and self.feats['Voice'] == 'Pass'):
            verb_state = 'VERB:pass'
        else:
            verb_state = 'VERB:act'
        
        self.rel_set.sort()
        if self.nsubj_pass is not None or self.nsubj is not None:
            self.rel_set = ['nsubj'] + self.rel_set
        self.rel_set = [verb_state] + self.rel_set
        
        s = '<'
        for string in self.rel_set:
            s+=string+','
        s = s[:-1] + '>'
        self.valence_category = s
        
    def __getitem__(self,item):
        return self.rel_set[item]
    
        
    def __repr__(self):
        return self.valence_category
    
    def __str__(self):
        return self.valence_category
    
    def print(self):
        verb = f'{self.lemma}'
        mdata = ''
        for key in ['Mood','Number','Person','Tense','VerbForm']:
            if key in self.feats:
                mdata+=f'+{key}:{self.feats[key]}'
        mdata = mdata + " "
        verb+=mdata
        if self.xcomp is not None:
            val = list(self.xcomp.keys())[0]
            if self.xcomp[val].upos in ('VERB'):
                xcomp = f"xcomp {val}+{self.xcomp[val].lemma}"
                for key in ['Mood','Number','Person','Tense','VerbForm']:
                    if key in self.xcomp[val].feats:
                        xcomp += f'+{key}:{self.xcomp[val].feats[key]}'
                xcomp += ' '
                for t in self.xcomp[val].relation:
                    xcomp += f'{t.deprel}+{t.upos}+{t.lemma} '

            else:
                xcomp = f"xcomp "
            verb+=xcomp
        #if self.ccomp is not None:
            #val = list(self.ccomp.keys())[0]
            #ccomp = f'ccomp {val}+{self.ccomp[val].deprel}+{self.ccomp[val].upos}+{self.ccomp[val].lemma} '
        #else:
            #ccomp = f'ccomp '
            #verb+=ccomp
        if self.ccomp is not None:
            val = list(self.ccomp.keys())[0]
            if self.ccomp[val].upos in ('VERB'):
                ccomp = f"ccomp {val}+{self.ccomp[val].lemma}"
                for key in ['Mood','Number','Person','Tense','VerbForm']:
                    if key in self.ccomp[val].feats:
                        ccomp += f'+{key}:{self.ccomp[val].feats[key]}'
                ccomp += ' '
                for t in self.ccomp[val].relation:
                    ccomp += f'{t.deprel}+{t.upos}+{t.lemma} '

            else:
                ccomp = f"ccomp "
            verb+=ccomp
        if self.obj is not None:
            if 'ADP' in self.obj.keys():
                if self.obj['ADP'].deprel == 'case':
                    verb += f"obj case+ADP+{self.obj['ADP'].lemma} "
                else:
                    verb += "obj "
            else:
                verb += "obj "
        if self.iobj is not None:
            if 'ADP' in self.iobj.keys():
                if self.iobj['ADP'].deprel == 'case':
                    verb += f"iobj case+ADP+{self.iobj['ADP'].lemma} "
                else:
                    verb += 'iobj '
            else:
                verb += 'iobj '
        if self.nsubj is not None:
            verb += f'nsubj '
        if self.csubj is not None:
            verb += 'csubj '
        if self.nsubj_pass is not None:
            verb += 'nsubj:pass '
        if self.aux_pass is not None:
            if 'AUX' in self.aux_pass.keys():
                aux = f"aux:pass:{self.aux_pass['AUX'].lemma}"
                for key in ['Mood','Number','Person','Tense','VerbForm']:
                    if key in self.aux_pass['AUX'].feats:
                        aux += f"+{key}:{self.aux_pass['AUX'].feats[key]}" 
                aux += ' '
            else:
                aux += 'aux:pass '
            verb += aux
                
        if self.expl is not None:
            if 'PRON' in self.expl.keys():
                expl = f"PRON+{self.expl['PRON'].token}+"
                for key in ['Case','Gender','Number','Person','PronType']:
                    if key in self.expl['PRON'].feats:
                        expl+=f"{self.expl['PRON'].feats[key]}+"
                expl = expl[:-1]
                verb += expl
        return verb
            

class Verb:
    def __init__(self,lemma=None, valences = []):
        self.lemma = lemma
        self.valences = valences
        
    def __repr__(self):
        return self.lemma
    
    def __str__(self):
        return self.lemma
    
    def add_valence(self,valence):
        if valence.lemma != self.lemma:
            raise TypeError("Not same lemma")
        self.valences.append(valence)

    def print(self):
        print_output = []
        for valence in self.valences:
            print_output.append(valence.print())
        print_output = list(set(print_output))
        s = ''
        for t in print_output:
            s+=t + "\n"
        return s
    #def __repr__(self):
        #s = f"{lemma}:{self.feats['mood']}+{self.feats['Number']}+{self.feats['Person']}+{self.feats['Tense']}+{self.feats['VerbForm']}\n"
        #if rels in self.rel.keys():
            
        #return None



## FUNÇÕES BÁSICAS
        

def get_root_index(sentence):
    for token in sentence:
        if token['deprel'] == 'root':
            return sentence.index(token)
        
def get_verbs_index(sentence):
    return [sentence.index(x) for x in sentence if x['upos'] == 'VERB']
        
        
def get_son_tokens(sentence,
                   token):
    token_id = token['id']
    tokens = [t for t in sentence if t['head'] == token_id]
    return tokens

def recover_verbs_valences(sentence,
                           with_lemmas=False):
    verbs = get_verbs_index(sentence)
    if with_lemmas:
        dic = {sentence[x]['lemma']:[(y['deprel'],y['lemma']) for y in get_son_tokens(sentence,sentence[x]) if y['deprel'] not in DEPRELS] for x in verbs}
        return dic
    dic = {sentence[x]['lemma']:[y['deprel'] for y in get_son_tokens(sentence,sentence[x]) if y['deprel']  in DEPRELS] for x in verbs} 
    return dic

def get_rel_set(sentence,token):
    tokens = get_son_tokens(sentence,token)
    rel_set = [x['deprel'] for x in tokens if x['deprel'] in DEPRELS and x['deprel'] != []]
    rel_set_aux = [x['id'] for x in tokens if x['deprel'] in DEPRELS and x['deprel'] != []]
    token_id = token['id']
    i = binary_search(token_id,rel_set_aux)
    rel_set = rel_set[:i] + ['VERB'] + rel_set[i:]
    return rel_set


## FUNÇÕES PARA EXTRAÇÃO


def get_deprel(sentence,token,deprel):
    son_tokens = get_son_tokens(sentence,token)
    son_tokens_deprel = [x['deprel'] for x in son_tokens]
    result_dic = {}
    if deprel not in son_tokens_deprel:
        return None
    else:
        deprels = [x for x in son_tokens if x['deprel'] == deprel]
        for deprel_ in deprels:
            result_dic[deprel_['upos']] = Relation(sentence,deprel_)
        return result_dic
       

def get_valence(sentence,token):
    obj = get_deprel(sentence,token,'obj')
    iobj = get_deprel(sentence,token,'iobj')
    ccomp = get_deprel(sentence,token,'ccomp')
    xcomp = get_deprel(sentence,token,'xcomp')
    expl = get_deprel(sentence,token,'expl')
    nsubj = get_deprel(sentence,token,'nsubj')
    csubj = get_deprel(sentence,token,'csubj')
    aux_pass = get_deprel(sentence,token,'aux:pass')
    nsubj_pass = get_deprel(sentence,token,'nsubj:pass')
    if obj is None and iobj is None and ccomp is None and xcomp is None and expl is None:
        return None
    return Valence(token = token['form'], 
                   lemma=token['lemma'],
                   feats=token['feats'], 
                   xcomp=xcomp,
                   ccomp=ccomp,
                   obj=obj,
                   iobj=iobj,
                   expl=expl,
                   nsubj = nsubj,
                   csubj=csubj,
                   nsubj_pass = nsubj_pass,
                   aux_pass = aux_pass,
                   example=sentence.metadata['text'],
                   rel_set = get_rel_set(sentence,token))
            
            
def main():
    verbs = {}
    with open("pt_bosque-ud-train.conllu") as arq:
        bosque = parse(arq.read())
    for sentence in bosque:
        for verb_index in get_verbs_index(sentence):
            verb_lemma = sentence[verb_index]['lemma']
            if verb_lemma not in verbs.keys():
                verbs[verb_lemma] = Verb(lemma=verb_lemma,valences = [])
            valence = get_valence(sentence,sentence[verb_index])
            if valence is None:
                continue
            else:
                verbs[verb_lemma].add_valence(valence)
        print(f'Done {(bosque.index(sentence)+1)*100/len(bosque):.3f}',end='\r')
    print("Done first part...")
    joblib.dump(verbs,'verbs_dict.joblib')
    d = verbs
    g = {}
    i=0
    for verb in d.keys():
        for valence in d[verb].valences:
            if str(valence) not in g.keys():
                g[str(valence)] = []
            if d[verb] not in g[str(valence)]:
                g[str(valence)].append(d[verb])
        i+=1
        print(f'Done {100*i/len(d.keys()):.2f}',end='\r')
    joblib.dump(g,'valences_dict.joblib')
    print('Done second part...')


def extract_example(valences, valence_category,lemma):
    examples=[]
    for verb in valences[valence_category]:
        if verb.lemma == lemma:
            for valence in verb.valences:
                if valence.valence_category == valence_category:
                    examples.append(valence.example)
    return examples
    
def extract_valences(file_path):
    verbs = {}
    with open(file_path) as arq:
        corpus = parse(arq.read())
    for sentence in corpus:
        for verb_index in get_verbs_index(sentence):
            verb_lemma = sentence[verb_index]['lemma']
            if verb_lemma not in verbs.keys():
                verbs[verb_lemma] = Verb(lemma=verb_lemma,valences = [])
            valence = get_valence(sentence,sentence[verb_index])
            if valence is None:
                continue
            else:
                verbs[verb_lemma].add_valence(valence)
    g = {}
    for verb in verbs.keys():
        for valence in verbs[verb].valences:
            if str(valence) not in g.keys():
                g[str(valence)] = []
            if verbs[verb] not in g[str(valence)]:
                g[str(valence)].append(verbs[verb])
    return g


def dump(d,out):
    with open(out, 'w') as f:
        for val in d.keys():
            for v in d[val]:
                print(val,v, file = f)
            

if __name__ == "__main__":
    main()
