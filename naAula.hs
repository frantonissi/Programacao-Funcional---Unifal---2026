import Data.Char

type Dia = Int
type Venda = Int
type  Periodo = Int

maxi::Int -> Int -> Int
maxi x y 
    | x >= y = x 
    | otherwise = y


f:: Dia -> Venda
f 1 = 9
f 2 = 7
f 3 = 15
f 4 = 14
f 5 = 8
f 6 = 0
f 7 = 3
f x = 0

maiorVenda:: Dia -> Venda -> Venda
maiorVenda 0 v = v 
maiorVenda x v = maxi (maiorVenda(x - 1) (f x)) v

maiorVenda1:: Periodo -> Venda 
maiorVenda1 0 = f 0
maiorVenda1 x = maxi (maiorVenda1 (x-1)) (f x)

diaMaiorVenda:: Dia -> Dia -> Venda
diaMaiorVenda 0 d = d
diaMaiorVenda x d 
    | f d >= f x = diaMaiorVenda (x - 1) d
    |otherwise = diaMaiorVenda (x - 1) x

diaVenda :: Venda->Dia
diaVenda v = diaVenda01 periodo v

diaVenda01 :: Periodo -> Venda -> Dia
diaVenda01 0 _ = 0
diaVenda01 dia v
    |f dia == v = dia
    |otherwise = diaVenda01 (dia -1) v