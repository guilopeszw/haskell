import Data.Char (toUpper)

monoAlphaCipherE :: [Char] -> String -> String -- define que recebe char e retorna String
monoAlphaCipherE key = map (charCriptografado key) 
-- em resumo: para cada caractere x da string, a função ira chamar a chave x
  where
    charCriptografado k c -- aqui, k é uma lista de 26 letras e c é o caractere a ser criptografado
      | c `elem` ['a'..'z'] = toUpper (k !! (fromEnum c - fromEnum 'a')) -- tratamento de letras minúsculas
      | c `elem` ['A'..'Z'] = toUpper (k !! (fromEnum c - fromEnum 'A')) -- tratamento de letras maiúsculas
      | otherwise = c -- se não for letra, ele retorna o caractere original 