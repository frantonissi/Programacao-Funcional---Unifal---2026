----------------------------------------SEMANA 02-------------------------------------------------


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
    |a == b = deleteList (b:as)
    |otherwise = a : deleteList (b:as)

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

--7
--8



-------------------------------------------------------------
{- Exercícios
     Implementar as funções: 
       myHead que recebe uma lista x e retorna a cabeça de x
       myTail que recebe uma lista x e retorna a lista x sem a cabeça
       myLast que recebe uma lista x e retorna o último elemento de x
       myInit que recebe uma lista x e retorna a lista x sem o último elemento

-}

myHead :: [Int] -> Int
myHead [] = 0
myHead (a:as) = a

myTail :: [Int] -> [Int]
myTail [] = []
myTail (a:as) = myTail as

myLast :: [Int] -> Int
myLast [a] = a  --Nao pode colocar só a do lado esquerdo do = porque aí o a do lado direito vira uma lista, e a funcao só retorna um Int
myLast (a:as) = myLast as

myInit :: [Int] -> [Int]
myInit [] = []
myInit [a] = []
--myInit [a,b] = [a]
myInit (a:as) = a : myInit as

----------------------------------------SEMANA 03-------------------------------------------------

maxi :: Int -> Int -> Int
maxi m n
    |m > n = m
    |otherwise = n

{- Assunto: listas e tuplas -}

periodo::Int
periodo = 7




-- tabela de vendas
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 0
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30

{- 01 função que retorna uma lista de vendas -}

listasVendas :: Int -> [Int]
listasVendas n
    |n == 0 = [vendas n]
    |otherwise = listasVendas (n - 1) ++ [vendas n]

{- 02 função que retorna [[Int]] com listas de dia e venda -}

f2L :: Int -> [[Int]]
f2L 0 = [   ]
f2L n =  f2L (n - 1) ++ [[n, vendas n]]

----------------------------------------------------------
{- 03 função que ordena uma lista de inteiros -}
passadaBubble::[Int]->[Int]
passadaBubble [] = []
passadaBubble [a] = [a]
passadaBubble (a:b:c)
    |a > b = b : passadaBubble (a:c)
    |otherwise = a : passadaBubble (b:c)


ordenaLista :: [Int] -> [Int]
ordenaLista a
    |a == passadaBubble a = a
    |otherwise = ordenaLista (passadaBubble a)


-------------------------------------------------------------------------
{- 04 função que ordena [[Int]] pelo primeiro Int de cada lista  -}
--ordenaListaLista::[[Int]]->[[Int]]


---------------------------------------------------------------------------
{- 05 função que ordena as listas internas de [[Int]] e, em seguida, ordena a [[Int]] -}
--ordenaLILE::[[Int]] ->[[Int]]


-----------  tuplas --------------------------------------------------------
{- 06 função que gera uma lista de tuplas com dia e venda -}
f6T :: Int-> [(Int, Int)]
f6T 0 = []
f6T x = (x,vendas x):f6T (x-1)

{- 07 função que gera o total de vendas-}
--totalVendasT::[(Int, Int)] -> Int

{- 08 função que retorna a maior venda -}

--maiorVendasT8a::Int-> [(Int, Int)] -> Int 

{- 08-b como implementar com apenas os parâmetros? -}
maiorVendaT8b::[(Int, Int)] -> Int
maiorVendaT8b [] = 0
maiorVendaT8b ((a,b):c) = maxi b (maiorVendaT8b c)

maiorVendaT8c::[(Int, Int)] -> Int
maiorVendaT8c [] = 0
maiorVendaT8c (a:c) = maxi (snd a) (maiorVendaT8c c)

maiorVendaT8d::[(Int, Int)] -> Int
maiorVendaT8d [] = 0
maiorVendaT8d c = maxi (snd (head c)) (maiorVendaT8d (tail c))

{- 09 função que retorna os dias das maiores vendas -}






































