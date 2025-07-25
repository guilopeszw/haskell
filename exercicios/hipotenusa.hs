-- where: definindo escopos locais
-- tudo dentro do where só vale para aquela função.
-- permite deixar o corpo principal mais limpo.
hipotenusa :: Floating a => a -> a -> a
hipotenusa a b = sqrt (c2) -- calcula a hipotenusa usando o teorema de Pitágoras
  -- sqrt calcula a raiz quadrada
  where
    c2 = a*a + b*b 

main :: IO ()
main = do
    putStrLn "Calculando a hipotenusa"
    putStrLn $ "hipotenusa 3 4 = " ++ show (hipotenusa 3 4) -- imprime o resultado da hipotenusa para os catetos 3 e 4
-- utilizando a função hipotenusa