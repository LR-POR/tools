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

      Verifica, para cada entrada da lista, se existe uma forma alternativa (forma diferente, lema e tags iguais). Se existir, a entrada é eliminada, se não existir, não é eliminada.

  * corLemma

      Input: path de um diretório do MorphoBr e ("lema atual","novo lema")

      Substitui o lema atual pelo lema novo em todas as entradas.

  * newLemma

      Input: path de um diretório do MorphoBr e novo lema

      Utilizando as regras lexicais da PorGram, produz e adiciona ao MorphoBr as flexões do novo lema. Caso o novo lema já exista no MorphoBr, apenas as entradas produzidas que não estavam inclusas são adicionadas, sem alterar o restante. 

  * alfaOrder

      Input: path de um diretório do MorphoBr e path para escrever os arquivos de saída

      Agrupa as entradas do diretório em arquivos de acordo com sua letra inicial

