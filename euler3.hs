divides x y = mod x y == 0
bignum = 600851475143
root = round(sqrt(600851475143))
dividesbignum x = divides bignum x

brutefactor :: Integral a => a -> a -> a
brutefactor x y = 
    if x `mod` y == 0 then y
    else brutefactor x (y-1)
    
foo = [x|x<-[2..bignum],dividesbignum x]

