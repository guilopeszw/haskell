▪ FUNÇÕES CURRIED: 
    Em Haskell, uma função curried é aquela que, em vez de receber todos os seus argumentos de uma vez, recebe-os um de cada vez, retornando uma nova função a cada passo até que todos os argumentos sejam fornecidos e a função final seja avaliada. Essa técnica é chamada de currying e é uma característica inerente da linguagem Haskell

    add' :: Int → (Int → Int)
    add' x y = x+y
        add' pega um inteiro x e retorna uma
        função add' x. Por sua vez, essa função
        pega um inteiro y e retorna o resultado x+y

        add e add' produzem o mesmo resultado final,
        mas add recebe seus dois argumentos ao mesmo
        tempo, enquanto add’ os recebe um de cada vez (uma é curried e a outra não)

    ▪ Funções com mais de dois argumentos podem ser curried retornando funções aninhadas:
        mult :: Int → (Int → (Int → Int))
        mult x y z = x*y*z
    
            mult pega um inteiro x e retorna uma função mult x, que por sua vez toma um inteiro y e retorna uma função mult y, que finalmente pega um inteiro z e retorna o resultado x*y*z.

            ou seja, mult x y z = ((mult x) y) z


    Por que currying é útil?
        As funções de curried são mais flexíveis do que as funções em tuplas, porque funções úteis muitas vezes podem ser feitas aplicando parcialmente uma função curried
        eg.:
            add’_1 :: Int → Int
            take_5 :: [Int] → [Int]
            drop_5 :: [Int] → [Int]

    A menos que tuplas sejam explicitamente necessárias, todas as funções em Haskell são normalmente definidas na forma curried

▪ FUNÇÕES POLIMÓRFICAS: 
    Uma função é chamada polimórfica ("de muitas formas") se seu tipo contém uma ou mais variáveis de tipo

    length :: [a] → Int
        Para qualquer tipo a, length usa uma lista de valores do tipo a e retorna um inteiro
    Variáveis de tipo podem ser instanciadas para tipos distintos em diferentes circunstâncias 

        > length [False,True]
        2           Aqui, a = Bool


        > length [1,2,3,4]
        4           Aqui, a = Int


        Nota:
            Variáveis de tipo devem iniciar com uma letra minúscula e em geral são nomeadas a, b, c, etc.

    Muitas das funções definidas no padrão prelude são polimórficas, por exemplo:
        fst :: (a,b) → a
        head :: [a] → a
        take :: Int → [a] → [a]
        zip :: [a] → [b] → [(a,b)]
        id :: a → a

▪ FUNÇÕES SOBRECARREGADAS: 
    Uma função polimórfica é dita sobrecarregada se seu tipo contiver uma ou mais restrições de classe

    (+) :: Num a => a → a → a
        Para qualquer tipo numérico a, (+) usa dois valores do tipo a e retorna um valor do tipo a

    Nota:
        Variáveis com restrições de classe podem ser instanciadas para qualquer tipo que as satisfaçam
        > 1 + 2
            3               a = Int

            > 1.0 + 2.0
            3.0             a = Float

            > 'a' + 'b'
            ERROR           dá erro pois Char não é numérico, logo, não pode ser somado. se fosse ++ poderia ser concatenado


        ▪ Haskell tem uma série de classes de tipos,
        incluindo
            - Num - Tipos numéricos
            - Eq - Tipos igualdade
            - Ord - Tipos ordenados
            Por exemplo
                (+) :: Num a => a → a → a
                (==) :: Eq a => a → a → Bool
                (<) :: Ord a => a → a → Bool

Dicas e sugestões
    ▪ Ao definir uma nova função em Haskell, é útil começar definindo seu tipo
    ▪ Em um script, é uma boa prática indicar o tipo de cada nova função definida
    ▪ Ao indicar os tipos de funções polimórficas que usam números, igualdade ou ordenações, tenha o cuidado de incluir restrições de classe necessárias