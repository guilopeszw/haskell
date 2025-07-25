-- Exercícios resolvidos do Capítulo 1

-- (1)

double :: Num a => a -> a
double x = x + x

-- Alternativa 1

-- double (double 2) 
-- = double (2 + 2) 
-- = (2 + 2) + (2 + 2) 
-- = 4 + (2 + 2) 
-- = 4 + 4 
-- = 8

-- Alternativa 2

-- double (double 2) 
-- = (double 2) + (double 2)
-- = (2 + 2) + (double 2) 
-- = 4 + (double 2) 
-- = 4 + (2 + 2) 
-- = 4 + 4
-- = 8

-- Existem outras opções

-- (2)

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + Main.sum xs

-- sum [x]
-- x + sum []
-- x + 0
-- x


-- (3)

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * Main.product xs

-- product [2,3,4]
-- 2 * product [3,4]
-- 2 * 3 * product [4]
-- 2 * 3 * 4 * product []
-- 2 * 3 * 4 * 1
-- 2 * 3 * 4
-- 2 * 12
-- 24

--(4)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
               where
                  ys = [a | a <- xs, a > x]
                  zs = [b | b <- xs, b <= x]
                  
-- [10, 4, 5, 8, 6, 11, 26]

--(4)

qsort' :: Ord a => [a] -> [a]
qsort' []     = []
qsort' (x:xs) = qsort' ys ++ [x] ++ qsort' zs
               where
                  ys = [a | a <- xs, a < x]
                  zs = [b | b <- xs, b > x]
                  
-- [2,2,3,1,1]