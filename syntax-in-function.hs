lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y


{-The _ means the same thing as it does in list comprehensions. 
It means that we really don't care what that part is, so we just write a _.-}
third :: (a, b, c) -> c
third (_, _, z) = z

-- let list = [1,2,3]
{-Abbreviation with-}
-- let list' = 1:2:3[]

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

{-Let's see what happens if we call length' on "ham". First, it will check if it's an empty list. 
Because it isn't, it falls through to the second pattern. 
It matches on the second pattern and there it says that the length is 1 + length' "am", 
because we broke it into a head and a tail and discarded the head. O-kay. 
The length' of "am" is, similarly, 1 + length' "m". 
So right now we have 1 + (1 + length' "m"). length' "m" is 1 + length' "" (could also be written as 1 + length' []). 
And we've defined length' [] to be 0. So in the end we have 1 + (1 + (1 + 0)).
-}
length' :: (Num b) => [a] -> b
length' [] = 0
-- length' (_:xs) = 1 + length' xs
langth' all@(_:xs) = all

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
-- capital "Darcula"

-- bmiTell :: (RealFloat a) => a -> String
-- bmiTell bmi
--     | bmi <= 18.5 = "You're underweight, you emo, you!"
--     | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
--     | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
--     | otherwise   = "You're a whale, congratulations!"

-- bmiTell' :: (RealFloat a) => a -> a -> String
-- bmiTell' weight height
-- 	| bmi <= 18.5 = "You're underweight, you emo, you!"    
-- 	| bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
--     | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
--     | otherwise   = "You're a whale, congratulations!"
--     where bmi = weight / height ^ 2

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
	| skinny = "You're underweight, you emo, you!"    
	| normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | fat    = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"     
    where bmi = weight / height^2
    	  skinny = bmi <= 18.5
    	  normal = bmi <= 25.0
    	  fat = bmi <= 30


checkNumber :: (RealFloat a) => a -> String
checkNumber a
	| a < 5.5      = "No"    
	| otherwise  = "Stupid"

max' :: (Ord a) => a -> a -> a
max' a b 
    | a > b     = a
    | otherwise = b


initials' :: (firstname, lastname) -> firstname
initials' (firstname,_) = firstname

--Nhận vào 2 chuỗi
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- (w,h) <- xs mean, take action xs and return to (w,h)
-- calcBmis [(5,6)] -> return (w,h) = (5,6) to calculate. ([bmi w h |...]) is output, afer | is qualifier
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi w h = w / h ^ 2      

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h^2, bmi >= 25.0]  

-- [let square x = x * x in (square 5, square 3, square 2)]
-- [(25,9,4)]

-- describeList xs = "The list is " ++ case xs of [] -> "empty."
-- 											   [x] -> "a singleton list."
-- 											   xs -> "a longer list."
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list." 
                                               xs -> "a longer list."




    