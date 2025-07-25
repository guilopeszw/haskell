n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]
main :: IO ()
main = do
    putStrLn "Calculando n"
    putStrLn $ "O valor de n é: " ++ show n
    putStrLn $ "O valor de a é: " ++ show a
    putStrLn $ "O tamanho da lista xs é: " ++ show (length xs)
    putStrLn $ "A lista xs é: " ++ show xs
  where
    a = 10
    xs = [1,2,3,4,5]