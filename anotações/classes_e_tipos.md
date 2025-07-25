Toda expressão bem formada tem um tipo, que pode ser calculado automaticamente em tempo de compilação, usando um processo chamado inferência de tipo

▪ Se avaliar uma expressão e produz um valor do tipo t, então e tem o tipo t, escrito:
e :: t

▪ Todos os erros de tipo são detectados em tempo de compilação, tornando os programas mais seguros e rápidos, evitando a necessidade de verificações de tipo em tempo de execução

▪ Bool - valores lógicos
▪ Char - caracteres únicos
▪ String - cadeias de caracteres
▪ Int - números inteiros
▪ Float - números de ponto flutuante

Assim como em Python e Java, em Haskell, é possível ter uma lista de listas
eg.: [['a'], ['b']]

▪ Uma tupla é uma sequência finita de componentes de tipos possivelmente diferentes
(False,True) :: (Bool,Bool)
(False,'a',True) :: (Bool,Char,Bool)

▪ (t1,t2,...,tn) é o tipo de n-tupla, com aridade n, cujos i-ésimos componentes têm tipo ti para qualquer i em 1..n

aridade = num de elementos de uma tupla

Uma função é um mapeamento de argumentos de um tipo para resultado de outro tipo
not :: Bool → Bool
even :: Int → Bool

t1 → t2 é o tipo de funções que mapeiam
valores do tipo t1 para valores para o tipo t2

nota:
    ▪ A seta → é digitada no teclado como ->
    ▪ Os tipos de argumento e do resultado são irrestritos, por exemplo, funções com vários argumentos ou resultados são possíveis usando tuplas ou listas

add :: (Int,Int) → Int
add (x,y) = x+y
zeroto :: Int → [Int]
zeroto n = [0..n]

NUM:
Integer, float e num

EQ:
é similar ao equalsTo do java. é uma classe que existe para comparar tipos. seus operadores são == e /=

Por que não é viável, em geral, que os tipos de função sejam instâncias da classe Eq?
-> Porque existem funções com tipos infinitos (Int -> Int) e Haskell é preguiçoso, então não vai avaliar as funções à menos que precise

Quando é viável? Dica: duas funções do mesmo tipo são iguais se sempre retornarem resultados iguais para argumentos iguais.
-> É viável para funções com domínio finito pequeno (booleans, por exemplo, que tem apenas True ou False) e para comparações de estruturas de funções (não para comparar seus resultados finais, mas apenas a construção de uma função)

