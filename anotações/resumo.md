# Introdução

### GHC e GHCi
- `GHC`: compilador Haskell.
- `GHCi`: interpretador interativo.
- Comando: `$ ghci`

### Prelude - Funções úteis:
```haskell
head [1,2,3]           -- 1  
tail [1,2,3]           -- [2,3]  
[1,2,3] !! 1           -- 2  
take 2 [1,2,3,4]       -- [1,2]  
drop 2 [1,2,3,4]       -- [3,4]  
length [1,2,3]         -- 3  
sum [1,2,3]            -- 6  
product [1,2,3]        -- 6  
reverse [1,2,3]        -- [3,2,1] 
```
### Aplicação de funções
```haskell
f a b + c*d   -- significa ((f a b) + (c*d))
```

### Scripts
```haskell
double x = x + x  
quadruple x = double (double x)  
```

### Comandos
- `:load meu_script.hs` => Carrega um arquivo Haskell no GHCi.
- `:reload` => Recarrega o script atual, útil após fazer alterações no arquivo .hs.
- `:type` => Mostra o tipo de uma expressão ou função.
- `:edit meu_script.hs` => Abre o editor configurado (padrão: sistema) para editar um arquivo Haskell.
- `:quit` => Sai do GHCi

### Regras
- Variáveis e funções começam com letra minúscula
- Identação importa


# Tipos e Classes

### Tipos básicos
- `Bool`
- `Char`
- `String`
- `Int`
- `Float`

### Listas e tuplas
```haskell
[1,2,3] :: [Int]  
[['a'],['b']] :: [[Char]]  
(False, 'a') :: (Bool, Char)
``` 

### Funções
```haskell
not :: Bool -> Bool  
add :: (Int, Int) -> Int  
add' :: Int -> (Int -> Int)  -- curried
```

#### Obs: Curried (ou currificada) é uma forma de escrever funções que recebem os argumentos um por vez. 
📌 Todas as funções em Haskell são curried por padrão!

📦 Por que usar funções curried?

✔ Flexibilidade: permite aplicação parcial de funções
```haskell
inc = add' 1   -- inc :: Int -> Int
inc 5          -- resultado: 6
```

### Polimorfismo
```haskell
length :: [a] -> Int  
id :: a -> a  
zip :: [a] -> [b] -> [(a,b)]  
```
Uma função pode operar sobre valores de diferentes tipos, mantendo a mesma definição.

Você escreve uma única função que funciona com vários tipos.

# Definindo funções

### Condicionais:
```haskell
abs n = if n >= 0 then n else -n  
```

### Funções protegidas (guarded):
```haskell
abs n | n >= 0 = n  
      | otherwise = -n  
```
Funções guarded (ou guardas) são uma forma de especificar condições para os resultados de uma função, sem usar if ... then ... else.

Elas funcionam como "casos condicionais" e tornam o código mais legível quando há múltiplas possibilidades de resultado.

### Pattern matching
Pattern Matching é o mecanismo de Haskell que permite comparar e decompor valores diretamente na definição da função. Em vez de escrever uma função com condicionais (if, case, etc.), você define casos específicos para diferentes formas de entrada.

🧠 Como funciona?

```haskell
not :: Bool -> Bool
not True  = False
not False = True
```

Não usamos if, só dizemos como not se comporta para cada padrão possível do tipo Bool.

```haskell
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)
```
Explicação:

Primeiro caso (n == 0): retorna 1 (caso base).

Segundo caso (n qualquer): usa recursão.

⚠️ Ordem importa: Haskell tenta casar o primeiro padrão. Se você inverter, vai causar erro para o caso 0.

### Expressões lambda:
```haskell
\x -> x + x  
add = \x -> (\y -> x + y)  
```

### Seções:
```haskell
(1+)    -- função sucessora  
(*2)    -- duplicação  
(/2)    -- divisão por 2  
(1/)    -- inverso  
```

# Listas
```haskell
[1,2,3]       :: [Int]
['a','b','c'] :: [Char]
[]            :: [a]         -- lista vazia
```

| Padrão   | Significado                                  |
| -------- | -------------------------------------------- |
| `[]`     | lista vazia                                  |
| `(x:xs)` | lista com pelo menos um elemento             |
| `(a,b)`  | tupla de dois elementos                      |
| `x`      | casa com qualquer valor (e dá nome a ele)    |
| `_`      | **corresponde a qualquer valor**, **ignora** |
| `(x:_)`  | pega a cabeça e ignora o resto               |


### Construção de uma lista
```haskell
[1,2,3]
```
Haskell lê isso como:
```haskell
1 : (2 : (3 : []))
```
#### O que isso quer dizer?
`:` (lê-se "cons") é o operador que junta um item com uma lista.

`[]` é a lista vazia, ou seja, o "fim da linha".

Então, ele começa colocando o 3 na lista vazia → 3 : []. Depois adiciona o 2 → 2 : (3 : []). Depois o 1 → 1 : (2 : (3 : []))

1 -> 2 -> 3 -> fim

No Haskell:

1 : é o primeiro bloco

2 : é o segundo

3 : é o terceiro

[] é o "fim" da lista

### Funções importantes dentro do Prelude:
```haskell
head    :: [a] -> a           -- retorna o primeiro elemento
tail    :: [a] -> [a]         -- retorna a cauda (sem o primeiro)
null    :: [a] -> Bool        -- testa se a lista está vazia
length  :: [a] -> Int         -- número de elementos
sum     :: Num a => [a] -> a  -- soma
product :: Num a => [a] -> a  -- produto
reverse :: [a] -> [a]         -- inverte a lista
```

### Indexação e slicing:
```haskell
[1,2,3] !! 0       -- 1 (indexação começa em 0)
take 2 [1,2,3,4]   -- [1,2]   (pega os 2 primeiros)
drop 2 [1,2,3,4]   -- [3,4]   (remove os 2 primeiros)
```

### Concatenação: 
```haskell
[1,2] ++ [3,4]     -- [1,2,3,4]
replicate 3 'a'    -- "aaa"
```

### Pattern matching em listas:
```haskell
f [] = ...              -- lista vazia
f (x:xs) = ...          -- lista não vazia (x = cabeça, xs = cauda)
```

(x:xs) significa: "lista com pelo menos um elemento", onde:

x é o primeiro elemento (cabeça)

xs é o restante da lista (cauda)

#### EXEMPLO:
```haskell
sumList [] = 0
sumList (x:xs) = x + sumList xs
```

## Compreensão de listas 
```haskell
[expressao | gerador, condição]
```` 

### EXEMPLO: 

```haskell
[x*2 | x <- [1..5]]               -- [2,4,6,8,10]
[x | x <- [1..10], even x]       -- [2,4,6,8,10]
[(x,y) | x <- [1,2], y <- [3,4]] -- [(1,3),(1,4),(2,3),(2,4)]
```

### Multiplos geradores:
```haskell
[(x,y) | x <- [1..2], y <- [3..4]]

[(x,y) | y <- [3,4], x <- [1,2]] -- muda a ordem dos pares
```
* se atentar à ordem, ela importa

### Geradores dependentes:
```haskell
[(x,y) | x <- [1..3], y <- [x..3]]
-- gera pares onde y >= x
```

```haskell
[x | x <- [1..10], even x]
-- só pega pares
```

## Funções com compreensão de lista
`factors` – fatores de um número:
```haskell
factors n = [x | x <- [1..n], n `mod` x == 0]
```

`prime` – verifica se é primo:
```haskell
prime n = factors n == [1,n]
```

`primes` – gera todos os primos até n:
```haskell
primes n = [x | x <- [2..n], prime x]
```

`concat` – junta uma lista de listas:
```haskell
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]
```

### Função zip:

A função zip pega duas listas e combina os elementos correspondentes em pares (tuplas).

```haskell
zip :: [a] -> [b] -> [(a, b)]
```
- Você dá uma lista de a e uma lista de b

- Ela retorna uma lista de tuplas ((a, b)), emparelhando o primeiro de cada, o segundo de cada, e assim por diante.

#### E se as listas forem de tamanhos diferentes?
```haskell
zip [1,2,3,4] ['a','b']
-- Resultado: [(1,'a'), (2,'b')]
``` 
- A função para quando a menor lista acabar. Os elementos restantes da maior lista são ignorados.

```haskell
zip [1,2] ['a','b']  -- [(1,'a'),(2,'b')]
```

`pairs` – pares consecutivos de uma lista:
```haskell
pairs xs = zip xs (tail xs)
```
essa função é ótima para checar a ordem (pares vizinhos)

`sorted` – verifica se lista está ordenada:
```haskell
sorted xs = and [x <= y | (x,y) <- pairs xs]
```
é semelhante ao .sort() de Python. Basicamente, vai retornar um bool informando se está na ordem crescente ou não

### Posição de elementos com zip:
A função zip é ótima para saber a posição de cada elemento de uma lista

```haskell
positions x xs = [i | (x',i) <- zip xs [0..], x == x']
```

#### EXEMPLO:
```haskell
positions 0 [1,0,0,1,0]  -- [1,2,4]
```

## Strings são listas
```haskell
"abc" == ['a','b','c']  -- True

length "haskell"   -- 7
zip "abc" [1,2,3]  -- [('a',1),('b',2),('c',3)]

count x xs = length [x' | x' <- xs, x == x'] -- conta quantas vezes um caractere aparece

count 's' "Mississippi"  -- 4
```

# Exercícios úteis

### 1. Dividir uma lista no meio:
```haskell
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2
```

### 2. Terceiro elemento de uma lista:
```haskell
third xs = xs !! 2
-- ou: third (_:_:x:_) = x
```

### 3. Tail segura (não explode com lista vazia):
```haskell
safetail xs
  | null xs   = []
  | otherwise = tail xs
```
| Conceito          | Ferramenta                           |                                  |
| ----------------- | ------------------------------------ | -------------------------------- |
| Extrair elementos | `head`, `tail`, `!!`, `take`, `drop` |                                  |
| Analisar listas   | `null`, `length`, `sum`, `product`   |                                  |
| Construir listas  | \`\[x                                | ...]`, `zip`, `++`, `replicate\` |
| Decompor listas   | Pattern Matching com `(x:xs)`        |                                  |
| Combinar listas   | `zip`, `concat`, compreensão         |                                  |
| Strings           | São apenas `[Char]`                  |                                  |
