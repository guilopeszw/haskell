-- Exemplos do Capítulo 1

add a b = a + b

inc = add 1

fat 0 = 1
fat n = n * fat (n -1)


-- Sequência de Fibonacci

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2) 
