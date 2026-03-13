invert::Int -> Int

invert x =  (x `mod` 10)*10^1 + (x `div` 10)*10^0--usando o mod ele sempre vai pegar o ultimo numero, e usando o div, sempre vai pegar o primeiro

main::IO()
main = do
    print(invert 54)
