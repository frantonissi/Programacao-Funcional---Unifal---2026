duplicate::String ->Int->String 
duplicate _ 0 = "" 
duplicate x b = x ++ duplicate x (b - 1)