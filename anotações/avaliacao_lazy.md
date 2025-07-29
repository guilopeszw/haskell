Lazy evaluation (avaliação preguiçosa) significa que Haskell só calcula algo quando você realmente precisa do resultado.


ex1:
soma :: Int
soma = 1 + 2
    aqui, a soma é apenas a expressão 1 + 2

print soma
    é apenas aqui que o Haskell irá calcular o valor da expressão

ex2:
infinita :: [Int]
infinita = [1..]  -- Lista infinita!

take 5 infinita
-- Resultado: [1,2,3,4,5]

Haskell não vai se preocupar com o resto da lista, pois ele sabe que só precisa pegar os 5 primeiros índices (não importa o que vem depois deles)


A avaliação lazy afeta a igualdade de funções porque permite que, em Haskell, tenhamos expressões não avaliadas e comportamentos que só se manifestam quando chamados - então você não tem acesso direto ao conteúdo interno de uma função. ela pode ser um "caixote fechado" com lógica que só aparece se for usada.

