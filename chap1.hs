-- Number of occurences of a char in a string
count :: Char -> String -> Int
count x []  = 0
count x (y:ys) | x == y = 1 + count x ys 
               | otherwise = count x ys

-- blowup function : abc => abbccc
blowup :: String -> String
blowup [] = []
blowup (x:xs) = blowup' 1 (x:xs)

blowup' :: Int -> String -> String
blowup' n [] = []
blowup' n (c:cs) = replicate n c ++ blowup' (n + 1) cs

-- lengths
lengths :: [String] -> [Int]
lengths [] = []
lengths x = map length x

sumLengths :: [String] -> Int
sumLengths [] = 0
sumLengths x = foldl (+) 0 (lengths x)
