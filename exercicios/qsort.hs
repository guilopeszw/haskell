qsort :: Ord a => [a] -> [a] -- Define a função qsort que recebe uma lista de elementos ordenáveis e retorna a lista ordenada

qsort [] = [] -- Caso base: se a lista for vazia, retorna uma lista vazia
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller -- Caso recursivo: separa o primeiro elemento (x) do resto da lista (xs)

    where
        smaller = [a | a <- xs, a <= x] -- Ordena recursivamente os elementos maiores que x, coloca x no meio, 
        --                                 e depois ordena os menores ou iguais a x
        larger  = [a | a <- xs, a > x]
        -- 'smaller' contém todos os elementos de xs menores ou iguais a x
        -- 'larger' contém todos os elementos de xs maiores que x

main :: IO () -- Função principal que executa o programa
main = do   
    putStrLn "Ordenando uma lista com qsort"
    let lista = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5] -- Define uma lista de números
    putStrLn $ "Lista original: " ++ show lista -- Imprime a lista original
    putStrLn $ "Lista ordenada: " ++ show (qsort lista) -- Imprime a lista ordenada usando qsort

