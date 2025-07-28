import Data.Char (toUpper)

chave :: [Char]
chave = "ZYNGWQAMXPKVULCEFRIBSJDOTH" -- define a chave de criptografia

monoAlphaCipherE :: [Char] -> String -> String -- *define que recebe char e retorna String
monoAlphaCipherE key = map (charCriptografado key) 
-- em resumo: para cada caractere x da string, a função ira chamar a chave x
  where
    charCriptografado k c -- aqui, k é uma lista de 26 letras e c é o caractere a ser criptografado
      | c `elem` ['a'..'z'] = toUpper (k !! (fromEnum c - fromEnum 'a')) -- tratamento de letras minúsculas
      | c `elem` ['A'..'Z'] = toUpper (k !! (fromEnum c - fromEnum 'A')) -- tratamento de letras maiúsculas
      | otherwise = c -- se não for letra, ele retorna o caractere original 

monoAlphaCipherD :: [Char] -> String -> String
monoAlphaCipherD key = map (charDescriptografado key)
  where
    charDescriptografado k c
      | c `elem` ['a'..'z'] = toAlpha k (toUpper c)
      | c `elem` ['A'..'Z'] = toAlpha k c
      | otherwise = c
    toAlpha k ch =
      case lookup ch (zip (map toUpper k) ['A'..'Z']) of
        Just plain -> plain -- aqui é efetuada uma verificação para ver se o caractere criptografado está na chave
        Nothing -> ch
            