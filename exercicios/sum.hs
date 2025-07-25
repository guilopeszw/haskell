{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

sum :: Num a => [a] -> a -- é a assinatura da função sum, que recebe uma lista de números e retorna um número
-- A assinatura indica que a função pode receber qualquer tipo numérico, como Int, Float ou Double.

sum [] = 0 -- aqui, indicamos que a soma de uma lista vazia é 0
-- A lista vazia é o caso base da recursão, ou seja, quando não há mais elementos para somar, a função retorna 0.
-- Isso é importante para evitar loops infinitos e definir o comportamento da função quando não há elementos

sum (x:xs) = x + Main.sum xs -- aqui, usamos a recursão para somar o primeiro elemento da lista (x)
--                                                             com a soma do restante da lista (xs)
-- (x:xs) é uma notação que indica que x é o primeiro elemento da lista e xs é o restante da lista.