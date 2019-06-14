import Data.List 

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub		

-- listIntersperse :: a -> [a] -> [a]
-- listIntersperse _ [] = []
-- listIntersperse _ [x] = [x]
-- listIntersperse ele (x:xs) = x : ele : listIntersperse ele xs

intersperse             :: a -> [ a ] -> [ a ]
intersperse _   []      = []
intersperse _   [ h ]   = [ h ]
intersperse sep (h:t)  = h : sep : intersperse sep t
