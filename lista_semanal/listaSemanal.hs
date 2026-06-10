
{-Objetivo da aula 
  exercícios e introdução à List Comprehension-}

import Data.Char

-- List comprehension---------------------------------------

{-multiplica todos elementos pares da lista pela escalar -}
f1 :: Int -> [Int] -> [Int]
f1 x a = [x * y|y <- a, y `mod` 2 == 0 ]

{-dado um booleano, decide por multiplicar todos elementos por 2 ou somar todos elementos com 5 -}
f2 :: Bool -> [Int] -> [Int]
f2 x a
    |x = [2 * y|y <- a]
    |otherwise = [5 + y|y <- a]

{-fazer a função f0 que agrupa em duplas os cabeças de duas listas-}
f0 ::[t]->[u]->[(t,u)]
f0 [] [] = []
f0 (a:b) (c:d) = [(a,c)] ++ f0 b d


{-objetivo da computação dado em sala de aula:
  Filtrar o resultado de produto cartesiano 
  para gerar o resultado de f0-}  
{- gera o produto cartesiano entre duas listas -}  

--f3::[t]->[u]->[(t,u)]



{- desfaz a lista de duplas gerada por f3 -}
--f3_aux01 ::[(t,u)]-> ([t],[u])


{-conta as ocorrências repetidas de fst(dupla) 
  da lista gerada por f3 -}
--f3_count ::(Eq t)=>[(t,u)]-> Int


{-solução que gera lista de listas de duplas separando as listas com duplas com mesmo fst-}
--f3_newList::(Eq u)=> u-> [(u,t)]->[(u,t)]
--f3_newList a l = [b|b<-l, a==fst b]

--f3_newListDel::(Eq u)=> u-> [(u,t)]->[(u,t)]
--f3_newListDel a l = [b|b<-l, a/=fst b]

--f3_newListofList::(Eq u)=>[(u,t)]->[[(u,t)]]
--f3_newListofList   []      = []
--f3_newListofList   (a:x)   = (f3_newList (fst a) (a:x)):f3_newListofList (f3_newListDel (fst a) (a:x))

{- função que gera o filtro no produto cartesiano 
   -------aqui, tem algo a ser ajustado---------}
   
--f3_filtra::Int->[[(u,t)]]->[(u,t)]
--f3_filtra   _   [] = []
--f3_filtra     i (a:b)= busca i a:f3_filtra (i+1) b

--busca::Int->[(u,t)]->(u,t)
--busca i (a:b)
--  | i/=0      = busca (i-1) b
 -- | otherwise = a
  

------- Revisão -----------------------------------------------------

{-01 função que retorna lista de duplas com char e posição na ASCII -}

listaDuplaCharInt:: Int-> [(Char,Int)]
listaDuplaCharInt 0 = []
listaDuplaCharInt x = [(chr x, x)] ++ listaDuplaCharInt (x - 1)


{-02 função meuChr que pesquisa um char pelo int na lista gerada -}

meuChr :: Int -> Char
meuChr x = meuChr_aux x (listaDuplaCharInt 126)

meuChr_aux :: Int -> [(Char, Int)] -> Char
meuChr_aux x (a:b)
    |x == snd(a) = fst(a)
    |otherwise = meuChr_aux x b

{-03 função meuOrd que pesquisa o int pelo char na lista gerada -}
meuOrd :: Char -> Int
meuOrd x = meuOrd_aux x (listaDuplaCharInt 126)

meuOrd_aux :: Char -> [(Char, Int)] -> Int
meuOrd_aux x [] = error "Caracter nao encontrado"
meuOrd_aux x (a:b)
    |x == fst(a) = snd a
    |otherwise = meuOrd_aux x b

{-04 função que ordena uma lista de inteiros -}
ordenaLista::[Int]->[Int]
ordenaLista [] = []

{-05 seja o tipo [(Bool, [Int])]. 
Faça uma função que ordena [Int] quando o booleano é True. 
Também, passe o Bool para False, quando ordenar [Int]
exemplo: ordenaListaDupla [(True,[3,4,1,0,9]),(False,[]),(True,[4,3,2,1,0])]
retorna:                  [(False,[0,1,3,4,9]),(False,[]),(False,[0,1,2,3,4])]
-}

ordenaListaDupla::[(Bool, [Int])]->[(Bool, [Int])]
ordenaListaDupla [] = []









































