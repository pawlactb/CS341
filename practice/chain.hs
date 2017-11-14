chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (div n 2)
    | odd n  = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
