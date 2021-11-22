# tools

* src/Irregs.hs

  * mkIrregsTab

      Input: path do diretório "verbs" do MorphoBr e path onde será escrita a saída da função

      Inicialmente é criado um dicionário onde as chaves são os lemas, cada qual associado a uma lista de tuplas do tipo (forma,regra). As tuplas de cada lema são analisadas individualmente pela função isRegular, que possui três saídas possíveis: 

      * lista vazia, ocorre quando a forma é regular;

      * lista com dois elementos, ocorre quando a forma em questão é irregular mas também existe uma forma regular para a mesma regra;

      * lista com um elemento, ocorre quando a forma é irregular e não existe uma regular alternativa

      Todas as saídas produzidas pela função isRegular formam a tabela de irregulares.



* src/MorphoTools.hs

  * delete

      Input: path de um diretório do MorphoBr e path de um arquivo com as entradas a serem eliminadas

      Verifica, para cada entrada da lista, se existe uma forma alternativa (forma diferente, lema e tags iguais). Se existir, a entrada é eliminada, se não existir, não é eliminada. As entradas são eliminadas de forma que a estrutura do arquivo é refeita, isso torna inutilizável o diff dos arquivos. A função é eficiente em memória utilizada.

  * deleteF

      Input: path de um diretório do MorphoBr e path de um arquivo com as entradas a serem eliminadas

      Eliminada as entradas sem verificar se existe uma forma alternativa. A estrutura do arquivo é preservada, fazendo com que a função não seja eficiente em alocação de memória. Consequentemente, a função não suporta eliminar muitas entradas de uma vez.

  * corLemma

      Input: path de um diretório do MorphoBr e ("lema atual","novo lema")

      Substitui o lema atual pelo lema novo em todas as entradas.

  * newLemma

      Input: path de um diretório do MorphoBr e novo lema

      Utilizando as regras lexicais da PorGram, produz e adiciona ao MorphoBr as flexões do novo lema. Caso o novo lema já exista no MorphoBr, apenas as entradas produzidas que não estavam inclusas são adicionadas, sem alterar o restante. 

  * alfaOrder

      Input: path de um diretório do MorphoBr e path para escrever os arquivos de saída

      Agrupa as entradas do diretório em arquivos de acordo com sua letra inicial



* src/Clean.hs

  * clean

      Input: path do diretório "verbs" do MorphoBr 

      Elimina três tipo de entradas espúrias: formas do infinitivo terminadas em (á|ê|ô|i|í); formas sem a feature +IMP+ se a primeira ou a segunda pessoa do plural não terminar em s; sejam (forma1,feats1) e (forma2,feats2) de um dado lema, onde feats1==feats2, feats1 termina em 2+SG, forma1 != forma2 e forma1 ou forma2 termina em s, eliminar a forma duplicada que não termina em s.

  * checkDelete

      Input: path de um diretório do MorphoBr e path de um arquivo com as entradas a serem eliminadas

      Verifica se as entradas possuem um equivalente no MorphoBr, caso uma entrada não possua, é retornado no terminal. 

  * notClitic

      Input: path de um diretório do MorphoBr, path de um arquivo com as entradas a serem analisadas, path onde será escrita a saída da função

      Verifica se as entradas analisadas (sem pronome) existem no diretório de clíticos do MorphoBr.

  * corAsseis

      Input: path do diretório de verbos do MorphoBr

      COrrige o sufixo -asseis para -ásseis

  * addVtag

      Input: path do diretório de clíticos do MorphoBr

      Adiciona a feature +V onde ela ainda não foi incluída.
