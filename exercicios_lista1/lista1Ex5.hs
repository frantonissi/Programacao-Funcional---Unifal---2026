square :: Int -> Int
square x = x*x


fourPower :: Int -> Int

fourPower x = square(square x)


main :: IO()

main = do
    print(fourPower 2)