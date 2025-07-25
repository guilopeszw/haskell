produtoLista :: Num a => [a] -> a
produtoLista [] = 1
produtoLista (x:xs) = x * produtoLista xs
main :: IO ()
main = do
    putStrLn "Calculando o produto de uma lista"
    let lista = [2, 3, 4]
    putStrLn $ "Produto da lista " ++ show lista ++ " = " ++ show (produtoLista lista)
    -- Exemplo de uso da função produtoLista
    putStrLn $ "Produto de [2, 3, 4] = " ++ show (produtoLista [2, 3, 4])