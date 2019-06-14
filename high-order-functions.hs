import Prelude hiding (map)
--https://stackoverflow.com/questions/21028908/ambiguous-occurrence-map-error-in-winhugs-haskell


-- max nhận vào một a rồi trả lại (tức là dấu ->) một hàm, mà hàm này nhận một a rồi trả lại một a.
-- Đó là lý do tại sao kiểu được trả lại cũng như các tham số của hàm chỉ đơn giản được ngăn cách bởi dấu mũi tên.
max' :: (Ord a) => a -> (a -> a) -- VD: 4 -> (4, a) -> (4,5)
max' a b 
    | a > b     = a
    | otherwise = b

multThree :: (Num a) => a ->(a -> (a -> a))
multThree x y z = x * y * z    

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

-- applyTwice (++ " HAHA") "HEY"
-- applyTwice ("HAHA " ++) "HEY"
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


-- zipWith' (+) [4,2,5,6] [2,6,2,3] 
-- zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- zipWith' (+) [4,2,5,6] [2,6,2,3] 
{-						 	        = (+) 4 2 : zipWith' (+) [2,5,6] [6,2,3] 
									= [6]: (+) 2 6 : zipWith' (+) [5,6] [2,3]
									= [6,8]: (+) 5 2 : zipWith' (+) [6] [3]
									= [6,8,7]: (+) 6 3 : zipWith' (+) [] []
									= [6,8,7,9] -}



-- flip' (zip [1,2,3,4,5] "hello") -> flip' (zip ([1,2,3,4,5], "hello"))
-- -> [('h',1),('e',2),('l',3),('l',4),('o',5)]
flip' :: (a -> b -> c) -> (b -> a -> c) -- nhan vao zip [1,2,3,4,5] "hello"
flip' f = g
    where g x y = f y x --zip ([1,2,3,4,5], "hello") = zip ("hello", [1,2,3,4,5])
    --    g x y       =   f y x
    --    g x y       =   f ((y::a) (x::b))
-- g ((x::b) (y::a))  =   f ((y::a) (x::b))

-- flip' :: (a -> b -> c) -> b -> a -> c
-- flip' f y x = f x y

--MAP DEFINITION: map f [a, b ,c] = [f(a), f(b), f(c)]
--map (map (^2)) [[1,2],[3,4,5,6],[7,8]] =  [[1^2, 2^2] [..] [...]]
--result: 
--[[1,4],[9,16,25,36],[49,64]]
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
-- [x+3 | x <- [1,5,3,1,6]]

--largestDivisible [non-argument]-> 99554
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

--sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
--sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])
--166650    

--COLLATZ CONJECTURE
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15    
    --        = length (filter isLong [chain(1)..chain(100)])
    --        = "isLong list"


-- let listOfFuns = map (*) [0..]
-- (listOfFuns !! 4) 5 -- -> 20    
-- Getting the element with the index 4 from our list returns a function that's equivalent to (4*). 
-- And then, we just apply 5 to that function. So that's like writing (4*) 5 or just 4 * 5.

-- foldl FUNCTION (Left fold)
-- Ex: sum' [3,5,2,1] -> 11
--foldl stand for Fold left.
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
--      = foldl (\acc x -> acc + x) 0 [3,5,2,1]
--      = foldl (\acc 3 -> acc + 3) 3 [5,2,1] 
--      = .....
--      = 11
-- [3,5,2,1] = x:xs -> left foremost element is x 
-- return acc +3 replace starting value.

{-IDENTICAL FUNCTION-}
-- sum' :: (Num a) => [a] -> a  
-- sum' = foldl (+) 0  
-- The lambda function (\acc x -> acc + x) is the same as (+). We can omit the xs as the parameter because calling foldl (+) 0 will return a function that takes a list.
-- Generally, if you have a function like foo a = bar b a, you can rewrite it as foo = bar b, because of currying.

-- STARTING VALUE is False as you can see. 
--\acc don't have specific meaning. it's only use for mark lamda expression. Merely acc play a role like variable
--After get TRUE -> acc = true -> REPLACE to False == STARTING VALUE
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- foldl FUNCTION (Right fold)
-- If we're mapping (+3) to [1,2,3]
map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs 
      --  = foldr (\x acc -> (+) x : acc) [] [1,2,3]
      --  = foldr (\x [3] -> (+) 3 : [3]) [6] [1,2,6]
      --  = foldr (\x [2] -> (+) 3 : [2]) [5,6] [1,5,6]
      --  = ...
      --  = [4,5,6]
      -- (3 : [3]) return [3+3] (NOTE: Cons operator)