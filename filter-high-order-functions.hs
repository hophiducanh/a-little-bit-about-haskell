import Prelude hiding (filter)

-- In high-order-functions can't excecuted at bottom. I don't know. Fuck
--FILTER SYNTAX: filter condition(><= x) predicate([x..])
--filter (>3) [1,5,3,2,1,6,4,3,2,1]
-- -> [5,6,4]
-- filter p xs = [ x | x <- xs, p x]
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) 
    | p x       = x : filter p xs
    | otherwise = filter p xs

--filter will iterate through whole input iterator while takewhile will break once the predicate turn False,
--if you have an iterator with 1st element that false to predicate, takewhile will break at 1st iteration and return empty
--sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
--sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])
--166650    
