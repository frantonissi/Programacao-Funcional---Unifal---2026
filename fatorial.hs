fat::Int -> Int
fat 0 = 1
fat x = x * fat(x -1)

main ::IO()
main = print (fat 5)