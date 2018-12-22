-- Exercise 4.44

-- lexographical comparison
compareLists :: Ord a => [a] -> [a] -> Ordering
compareLists []      (_:_)   = LT
compareLists []      []      = EQ
compareLists (_:_)   []      = GT
compareLists (x:xs)  (y:ys)  = compareAux x y (compareLists xs ys)

compareAux :: Ord a => a -> a -> Ordering -> Ordering
compareAux x y o = 
    case compare x y of 
        EQ -> o
        LT -> LT
        GT -> GT

-- shorter lists < longer lists
-- for equal length, compare first elements, and if they match compare the rest        
compareListLen :: Ord a => [a] -> [a] -> Ordering
compareListLen []       (_:_)   = LT
compareListLen []       []      = EQ
compareListLen (_:_)    []      = GT
compareListLen x        y       = 
    case compare (length x) (length y) of
        LT -> LT
        GT -> GT
        EQ -> compareAux x y (compareListLen (tail x) (tail y) )        


{-
Test output:
*Main> :load chap4.hs
[1 of 1] Compiling Main             ( chap4.hs, interpreted )
Ok, one module loaded.
*Main> let a = "ABC"
*Main> let b = "ABCD"
*Main> compareListLen a b
LT
*Main> let a = "ABCE"
*Main> compareListLen a b
GT
*Main> let a = "ABCDE"
*Main> compareListLen a b
GT
-}        


-- Exercise 4.46
-- Reverse a list
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList(xs) ++ [x]

-- Reverse a list - v2
reverseList2 :: [a] -> [a]
reverseList2 [] = []
reverseList2 x = foldl (\ acc y -> y : acc) [] x

-- Exercise 4.47
-- splitList
-- splitList [1..4] => [ ([1],[2,3,4]), ([1,2],[3,4]), ([1,2,3],[4]) ]
splitList :: [a] -> [([a],[a])]
splitList [] = []
splitList x = splitList' 1 x

splitList' :: Int -> [a] -> [([a],[a])]
splitList' _ [] = []
splitList' i x = 
    -- check for i+1 so the last value does not have an empty list as the second element
    -- don't want to recursively call when i+1 = length x
    case i+1 < length x of 
        True -> [splitAt i x] ++ splitList' (i+1)  x
        False -> [splitAt i x]
