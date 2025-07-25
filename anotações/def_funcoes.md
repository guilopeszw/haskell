▪ EXPRESSÕES CONDICIONAIS:
    Como na maioria das linguagens de programação, funções podem ser definidas usando expressões condicionais

        abs :: Int → Int
        abs n = if n ≥ 0 then n else -n
            abs pega um inteiro n e retorna n se ele for não-negativo e -n caso contrário

        em haskell, expressões condicionais podem ser alinhadas:
            signum :: Int → Int
            signum n = if n < 0 then -1
                else if n == 0 then 0
                    else 1
        
        Em Haskell, expressões condicionais devem sempre ter a cláusula else, o que evita possíveis problemas de ambiguidade com condicionais aninhados
▪ EQUAÇÕES PROTEGIDAS (GUARDED):
    Como alternativa às expressões if, funções podem ser definidas usando guarded equations

    abs n | n ≥ 0 = n 
          | otherwise = -n
    Como anteriormente, mas usando guarded equations

    Equações protegidas podem ser usadas para facilitar a leitura de definições envolvendo várias condições
        signum n | n < 0 = -1
                 | n == 0 = 0
                 | otherwise = 1
    obs: A condição em qualquer outro caso otherwise é definida no prelúdio por otherwise = True

▪ CASAMENTO DE PADRÕES:
    Muitas funções têm uma definição particularmente clara usando casamento de padrões (pattern matching) em seus argumentos

    not :: Bool → Bool
    not False = True
    not True = False

    Funções podem ser definidas de muitas maneiras diferentes usando a casamento de padrões, por exemplo
        (&&) :: Bool → Bool → Bool
        True && True = True
        True && False = False
        False && True = False
        False && False = False
    pode ser definida de forma mais compacta por
        True && True = True
        _ && _ = False
    
    Entretanto, a definição a seguir é mais eficiente, pois evita avaliar o segundo argumento se o primeiro argumento for Falso:
        True && b = b
        False && _ = False          
            O símbolo de sublinhado _ é um padrão curinga (wildcard) que corresponde a qualquer valor de argumento

▪ PADRÕES EM LISTAS:
    Funções em listas podem ser definidas usando o padrão x:xs
        head :: [a] → a
        head (x:_) = x
        tail :: [a] → [a]
        tail (_:xs) = xs

    O padrão x:xs deve ser colocado entre parênteses porque a aplicação da função tem prioridade sobre (:), por exemplo, a seguinte definição resulta em erro
    head x:_ = x

▪ FUNÇÕES LAMBDA:
    São funções anônimas, que são a base da programação funcional. Expressões lambda podem ser usadas para dar um significado formal às funções definidas usando currying

        add :: Int → Int → Int
        add x y = x + y
        ==
        add :: Int → (Int → Int)
        add = x → (y → x + y)

        Expressões lambda podem ser usadas para evitar nomear funções que são referenciadas apenas uma vez

▪ SEÇÕES DE OPERADOR:
    Um operador escrito entre seus dois argumentos pode ser convertido em uma função curried escrita antes de seus dois argumentos, usando parênteses

    > 1+2
    3

    > (+) 1 2
    3

    Essa convenção permite que um dos argumentos do operador seja incluído entre parênteses. Por exemplo:
    > (1+) 2
    3
    > (+2) 1
    3

    Por que seções são úteis?
    Funções úteis às vezes podem ser construídas de maneira simples usando seções, por exemplo:
    (1+) - função sucessora
    (1/) - função de reciprocidade
    (*2) - função de duplicação
    (/2) - função de redução pela metade

    