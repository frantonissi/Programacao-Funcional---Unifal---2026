import Data.Char

anyDigit :: Int -> Int -> Int
anyDigit a b
    | a > length (show b) = -1
    | otherwise = digitToInt (show b !! a) --Para acessar um índice específico de uma lista ou string em Haskell, usamos o operador !!. Por exemplo: "7689" !! 2 vai te devolver o caractere '8'.
