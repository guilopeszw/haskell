Função:	a -> b	=> Precisa ser aplicada a argumentos para gerar um valor       	f 3 = 6
Valor:	a (ou Num a => a)	=> Já é um resultado computado	pi = 3.1415

PADRÃO PRELUDE
    Biblioteca padrão de Haskell com grande número de funções. Além das funções numéricas familiares, como + e *, a biblioteca também fornece muitas funções úteis em listas

COMANDOS:
head: seleciona primeiro item de uma lista
tail: remove o primeiro item de uma lista
!! n: seleciona o enésimo item de uma lista -> [1,2,3,4] !! 3 => 4
take n: seleciona os primeiros n elementos de uma lista
drop n: remove os primeiros n elementos de uma lista
length: calcula o tamanho de uma lista
sum: calcula a soma de uma lista de números
product: calcula o produto de uma lista de números
ls ++ ls: concatena duas listas
reverse ls: inverte a ordem de uma lista

:load name carrega script name
:reload recarrega script atual
:set editor name define editor para name
:edit name edita script name
:edit edita script atual
:type expr mostra o tipo de expr
:? Mostra todos os comandos
:quit sai do GHCi

▪ Nomes de função e argumento devem começar com letra minúscula, por exemplo:
myFun, fun1, arg_2

▪ Por convenção, argumentos de lista têm um sufixo s em seu nome, por exemplo:
xs, ns, nss

