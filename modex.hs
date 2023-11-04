module Modex where 


-- Use an efficient modular exponentiation algorithm.
-- m = modulus
-- b = base
-- k = exponent
-- works in the opposite order of python's pow(x, y, modulus)
expm m b k =
    let
        ex a k s
            | k == 0 = s
            | k `mod` 2 == 0 = ((ex (a*a `mod` m)) (k `div` 2)) s
            | otherwise = ((ex (a*a `mod` m)) (k `div` 2)) (s*a `mod` m)
    in ex b k 1
    
