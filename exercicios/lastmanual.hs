ls = [1,2,3,4,5]

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs
myLast [] = error "Lista vazia"

main :: IO ()
main = do
    putStrLn "Exercício: Encontrar o último elemento de uma lista"
    putStrLn $ "Lista: " ++ show ls
    putStrLn $ "Último elemento: " ++ show (myLast ls)
    putStrLn $ "Último elemento de [1,2,3,4,5]: " ++ show (myLast [1,2,3,4,5])