sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 
    map (\x -> x * 2 + 1) $ filter (\x -> not (x `elem` [ i+j+2*i*j | i <- [1..n], j <- [i..n], i+j+2*i*j <= n ])) [1..n]
