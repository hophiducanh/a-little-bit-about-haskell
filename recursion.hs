head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' (x:xs) = xs

-- [2,5,1]
-- maximum' :: (Ord a) => [a] -> a
-- maximum' [x] = x {-head-}
-- maximum' (x:xs)
-- 	| x > maxTail = x
-- 	| otherwise = maxTail
-- 	where maxTail = maximum' xs {-tail-}
--  maximum' xs = right foremost

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
-- How's that for elegant! In essence, the maximum of a list is the max of the first element and the maximum of the tail.


--n <= 0 dùng cả Num và Ord. [a] -> store return value. Vd replicate' 3 5 -> [5,5,5]. return [x] -> [x,x] -> [x,x,x]. 
--return value before cons operator :
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x:take' (n-1) xs    

-- [1,2,3] -> [2,3] ++ 1 -> 3 ++ 2 ++ 1
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs
-- elem' a xs OR a `elem'` xs  

-- In function everytime you see function name meant you call recursive
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted

-- quicksort :: (Ord a) => [a] -> [a]  
-- quicksort [] = []  
-- quicksort (x:xs) =   
--     let smallerSorted = quicksort (filter (<=x) xs)  --FILTER
--         biggerSorted = quicksort (filter (>x) xs) 
--     in  smallerSorted ++ [x] ++ biggerSorted 