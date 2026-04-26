{-
{- 01 função que soma os elementos de uma lista -}
sumList::[Int]->Int
sumList [] = 0

{- 02-localiza elemento em lista -}
searchList::Int->[Int]->Bool
searchList _ _ = False

{-03 remove todas ocorrências de y em uma lista -}
deleteList::Int->[Int]->[Int]
deleteList _ _ = []

{-04 informa o tamanho de uma lista -}
lenghtList::[Int]->Int
lenghtList _ = 0

{-05 conta a ocorrência de um Int em [Int] -}
contList::Int->[Int]->Int
contList _ _ = 0

{- 06 inverte a lista -}
reverseList:: [Int]->[Int]
reverseList _ = []

{- 07 inverte elementos das listas internas -}

{- 08 função que exclui a penúltima ocorrência de um número na lista-}

-}
-----------------------------------------------------------------------------------------------

--1
sumList :: [Int] -> Int
sumList [] = 0
sumList (a:b) = a + sumList b --cabeça + as novas cabeças que antigamente eram caudas, poe só b, porque b é uma lista, (o [Int] que o programa recebe), se colocasse [b], ai seria uma lista de lista

--2
searchList :: [Int] -> Int -> Bool
searchList [] _ = False
searchList (a:b) x
    |a /= x = searchList b x
    |otherwise = True

--3
deleteList::[Int]->[Int] --[1,2,2,3] -> [1,2,3]
deleteList [] = []
deleteList [a] = [a]
deleteList (a : b : as)
    |a == b = deleteList(b:as)  
    |otherwise = a : deleteList(b:as)

--4
lenghList:: [Int] -> Int
lenghList [] = 0
lenghList (a:b) = 1 + lenghList b

--5
contList::Int->[Int]->Int --1 [1,2,3,1] -> 2
contList _ [] = 0
contList x (a:as)
    |a == x = 1 + contList x as
    |otherwise = 0 + contList x as

--6
reverseList :: [Int] -> [Int] --[1,2,3] -> [3,2,1]
reverseList [] = []
reverseList (a:b) = reverseList b ++ [a]

duplica :: [Int] -> [Int]
duplica [] = []
duplica (a:as) = a : a : duplica as --a : (a : (duplica as)), o ':' serve pra pegar um elemento e colar na frente da lista, ou no fim

