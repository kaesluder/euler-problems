import Data.List
import qualified Data.Vector as V

step1a xs = filter (\(a,b) -> b) . zip [0..] $ zipWith (<) xs (tail xs)
step1 xs
    | length (step1a xs) == 0 = -1
    | otherwise = fst . last $ step1a xs
    
step2 a xs = fst. last . filter (\(a,b) -> b) . zip [0..] $ map (>(xs !! a)) xs

step3 a b xs =
    let x = xs !! a 
        y = xs !! b
        ys = changeIndex a y xs
    in changeIndex b x ys
    
    
step4 a xs =
    let z = a
        frontSection = takeToIndex z xs
        backSection = takeAfterIndex (z+1) xs
    in frontSection ++ (reverse backSection)

nextIteration xs
    | step1 xs < 0 = []
    | otherwise = 
        let a = step1 xs
            b = step2 a xs
            ys = step3 a b xs
        in step4 a ys

iterateHelper [] = Nothing
iterateHelper xs = Just (xs, nextIteration xs) 

changeIndex idx a xs =
    map passOrChange $ zip [0..] xs
        where passOrChange (x,y)
                | x == idx = a
                | otherwise = y

euler24 = last . take 1000000 $ unfoldr iterateHelper [0..9]

test4 = unfoldr iterateHelper [0..4]
                
takeToIndex idx xs =
    map snd $ takeWhile (\(x,y) -> x <= idx) $ zip [0..] xs

takeAfterIndex idx xs = 
    map snd $ dropWhile (\(x,y) -> x < idx) $ zip [0..] xs
    
main = putStrLn $ show euler24