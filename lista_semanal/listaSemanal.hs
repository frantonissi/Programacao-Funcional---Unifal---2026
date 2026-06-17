import Data.Char
{--As funções f1_p1 e f2_p2, a seguir, são específicas. A partir
de uma [String], elas geram [(Bool, String)] separando as strings
que só possuem dígitos das demais --}

--questão 01
f1_p1::String->Bool
f1_p1 [] = True
f1_p1 (x:xs)
  |isDigit1 x = f1_p1 xs
  |otherwise = False


-- questão 02
f2_p1::[String]->[(Bool, String)]
f2_p1 [] = []
f2_p1 (x:xs)
  |f1_p1 x = [(True,x)] ++ f2_p1 xs
  |otherwise = [(False, x)] ++ f2_p1 xs


isDigit1 x
  |ord x >= 48 && ord x < 58 = True
  |otherwise = False

--caso o alpha seja qualquer coisa menos um int--
isAlpha1 :: Char -> Bool
isAlpha1 x
  |ord x >= 48 && ord x < 58 = False
  |otherwise = True

{-generalizando a função f2_p1 -}
f2_p1_generica::(String->Bool)->[String]->[(Bool,String)]
f2_p1_generica x s = [(x a,a)|a <- s]
  
{- Mas, essas funções podem ser generalizadas.
 Pensemos em uma solução recebe uma [String] e retorna uma [(Bool, String)]
 em que o Bool é True se o String atende a uma característica determinada.
 Para tanto, devemos implementar as características desejaveis -}

cr01_Todos_Char::(Char->Bool)->String->Bool
cr01_Todos_Char f [a]     = f a
cr01_Todos_Char f (a:b)   = f (a) && cr01_Todos_Char f b

cr01_Algum_Char::(Char->Bool)->String->Bool
cr01_Algum_Char f [a]     = f a
cr01_Algum_Char f (a:b)   = f (a) || cr01_Algum_Char f b


{-observe que todos ou alguns podem ser generalizados, também. 
  Para isso, basta passar o operador lógico && ou || -}

cr01_A_T_Char::(Bool->Bool->Bool)->(Char->Bool)->String->Bool  
cr01_A_T_Char _ f [a]     = f a
cr01_A_T_Char o f (a:b)   = (o) (f (a)) (cr01_A_T_Char o f b)


{- as funções abaixo filtram de [(Bool, String)] a [String]
   de acordo com o Booleano -}
    
{-como transformá-las em uma função genérica? -}

filtraT::[(Bool, String)]->[String]
filtraT [] = []
filtraT (a:b)
  |fst a     = snd a:filtraT b
  |otherwise =       filtraT b
  
filtraF::[(Bool, String)]->[String]  
filtraF [] = []  
filtraF (a:b)
  |not(fst a) = snd a:filtraF b
  |otherwise  =       filtraF b

{--------------------solução---------------------}
filtraTF::(Bool->Bool)->[(Bool, String)]->[String]
filtraTF _ [] = []
filtraTF f (a:b)
  |f (fst a)     = snd a:filtraTF f b
  |otherwise =       filtraTF f b

seletor  x = x
inversor x = not(x)
{------------------------------------------------}

------------------  revisão e uso de alta ordem ----------------------------
{- Considere f1 capaz de somar uma lista de inteiros se um Char for alfanumérico, 
    ou multiplicar os elementos, caso contrário -}

f1::Char->[Int]->Int
f1 c x
  |isDigit (c)     && x==[]    = 0
  |not (isDigit c) && x==[]    = 1
  |isDigit c                   = a + f1 c b
  |otherwise                   = a * f1 c b
    where (a:b) = x

    
{- reescreva f1 usando casamento de padrão -}
--f2::Char->[Int]->Int

{- reescreva f2 fazendo chamadas de funções para somar ou multiplicar -}
--f3::Char->[Int]-> Int

{- reescreva f3 usando função de alta ordem para definir o operador
   Esta função é didática, pois mostra o uso de função de alta ordem
   Neste caso, considere que a lista tem, pelo menos, um elemento -}
 {- para lista de pelo menos um elemento -}
--f4::(Int->Int->Int)->[Int]->Int

{- faça a função myMap aplica uma função a cada elemento de uma lista -}

{- função que converte caixa baixa para caixa  alta
   usar a função myMap para aplicar a uma String -}
   










