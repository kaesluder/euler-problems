twoex = zip ns ds where
 ns = 3 : zipWith (\x y -> x + 2 * y) ns ds
 ds = 2 : zipWith (+) ns ds

len = length . show

main = print $ length $ filter (\(n,d) -> len n > len d) $ take 1000 twoex

