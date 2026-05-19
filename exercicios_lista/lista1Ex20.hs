tamanhoString :: [Char] -> Int
tamanhoString [] = 0
tamanhoString (a:b)  = tamanhoString b + 1



pushRight::String->Int->String
pushRight x y
    |tamanhoString x < y = ">" ++ pushRight x (y - 1)
    |otherwise = x

