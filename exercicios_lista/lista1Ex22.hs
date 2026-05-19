invertList :: [Int] -> [Int]
invertList [] = []
invertList (a:b) = invertList b ++ [a]