-- Chapter 4

-- 4.51 - listDiff
-- In set terms, this is the opposite of intersection
-- Another way to do this would be to calc the intersection, then remove the intersection from the union of the two sets
listDiff :: Eq a => [a] -> [a] -> [a]
listDiff [] ys = ys
listDiff xs [] = xs
listDiff (x:xs) ys | elem' x ys = listDiff xs (delete x ys)
                   | otherwise  = x : listDiff xs (delete x ys)

elem' :: Eq a => a -> [a] -> Bool
elem' x []                 = False
elem' x (y:ys) | x == y    = True
               | otherwise = elem' x ys

delete :: Eq a => a -> [a] -> [a]
delete x []                 = []
delete x (y:ys) | x == y    = ys
                | otherwise = y : delete x ys

