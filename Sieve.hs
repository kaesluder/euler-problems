module Sieve where 

-- euler12.hs
primesToQ m = 2 : sieve [3,5..m]  where
    sieve []     = []
    sieve (p:xs) = p : sieve (xs `minus` [p*p, p*p+2*p..m])

primesLME = 2 : ([3,5..] `minus` joinL [[p*p, p*p+2*p..] | p <- primes']) 
  where
    primes' = 3 : ([5,7..] `minus` joinL [[p*p, p*p+2*p..] | p <- primes'])
 
joinL ((x:xs):t) = x : union xs (joinL t)

primesPE = 2 : primes'
  where 
    primes' = sieve [3,5..] 9 primes'
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `minus` [q, q+2*p..]) (head t^2) t
primesTME = 2 : (gaps 3 $ joinT [[p*p, p*p+2*p..] | p <- primes']) 
  where
    primes' = 3 : (gaps 5 $ joinT [[p*p, p*p+2*p..] | p <- primes'])   
 
joinT ((x:xs):t) = x : union xs (joinT (pairs t))  
pairs ((x:xs):ys:t) = (x : union xs ys) : pairs t 
gaps k s@(x:xs) | k<x  = k:gaps (k+2) s    -- equivalent to
                | True =   gaps (k+2) xs   --  [k,k+2..]`minus`s, k<=head s
    
-- ordered lists, difference and union
minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

union (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : union  xs  (y:ys)
           EQ -> x : union  xs     ys 
           GT -> y : union (x:xs)  ys
union  xs     []    = xs
union  []     ys    = ys

