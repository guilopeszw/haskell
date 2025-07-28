# 📘 Lista de Questões de Estudo – Haskell (Capítulos 1 a 5)

---

### 1. O que diferencia uma linguagem funcional de uma linguagem imperativa? Dê exemplos de código para ilustrar.

**Resposta:**

Linguagens imperativas usam variáveis mutáveis e comandos sequenciais:

```java
int total = 0;
for (int i = 1; i <= 10; i++) {
  total += i;
}
```

Já linguagens funcionais usam funções puras:

```haskell
sum [1..10]
```

### 2. Defina a função recursiva que soma todos os elementos de uma lista.

```haskell
soma [] = 0
soma (x:xs) = x + soma xs
```

### 3. Usando compreensão de listas, crie uma função que retorne todos os números pares de 1 até n.
```haskell
pares n = [x | x <- [1..n], even x]
``` 

### 4. O que é uma função curried? Reescreva uma função que soma dois números como curried.

#### Resposta:
Função curried é uma função que recebe seus argumentos um por um.
```haskell
soma :: Int -> Int -> Int
soma x y = x + y
```

5. Dê um exemplo de função polimórfica e explique.
#### Resposta:
```haskell
length :: [a] -> Int
``` 
length funciona com listas de qualquer tipo (por exemplo, [Int], [Char], [Bool] etc).
Isso é chamado de polimorfismo paramétrico.

6. Escreva uma função usando guardas que classifica um número como negativo, zero ou positivo.
```haskell
classifica n
  | n < 0     = "Negativo"
  | n == 0    = "Zero"
  | otherwise = "Positivo"
```

### 7. O que a função zip faz? Dê um exemplo prático com strings.

#### Resposta:
a função zip cria uma lista que emparelha dois elementos, um de cada lista

```haskell
zip "abc" [1,2,3]  -- [('a',1), ('b',2), ('c',3)]
```

### 8. O que acontece se a função zip for usada com listas de tamanhos diferentes?

#### Resposta: 
A função para na menor lista:

```haskell
zip [1,2,3] ['a','b']  -- [(1,'a'), (2,'b')]
```

### 9. Escreva uma função que diga se uma lista está ordenada.

#### Resposta:
```haskell
sorted xs = and [x <= y | (x,y) <- zip xs (tail xs)]
```

### 10. Escreva uma versão reversa da função quicksort apresentada no capítulo 1.

#### Resposta:

```haskell
qsort [] = []
qsort (x:xs) = qsort maiores ++ [x] ++ qsort menores
  where
    maiores = [a | a <- xs, a > x]
    menores = [a | a <- xs, a <= x]
```