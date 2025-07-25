f :: Num a => a -> a -- pode indicar qualquer valor numérico
-- f é uma função que recebe um número e retorna outro número
f x = a * x + b -- f de x = a * x + b
  where -- especifica os valores de a e b
    a = 1
    b = 3

z :: Num a => a
z = f (2 + 3) -- z é uma variável que recebe o resultado de f aplicado a 2 + 3

main :: IO () -- função principal que executa o programa
-- IO () indica que a função não retorna um valor significativo, apenas executa ações de entrada    
main = do
  putStrLn "Hello world em haskell" -- imprime uma mensagem na tela
  putStrLn $ "f 2 = " ++ show (f 2) -- imprime o resultado de f aplicado a 2
  putStrLn $ "z = " ++ show z -- imprime o valor de z

-- utilizar f $ x é a mesma coisa que utilizar f (x), mas $ é mais legível e evita parênteses excessivos
-- ++ concatena duas strings/listas de caracteres
-- show converte um valor em uma string