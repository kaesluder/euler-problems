import Data.List.Split
getCipher = do 
     cipherText <- readFile "cipher1.txt"
     let cipherListText = map read $ splitOn "," cipherText
     return cipherListText 

