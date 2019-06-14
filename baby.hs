doubleMe x = x + x;

doubleUs x y = x*2 + y*2
-- doubleUs x y = x + x + y + y;
doubleUs x y = doubleMe x + doubleMe y
-- doubleUs x y =  doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
	then x
	else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

addString = "hello" ++ " " ++ "world"

takeCycle = take 10 (cycle [1,2,3])

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

composeList = [x*2 | x <- [1..10]]

{-elem nhận vào một thứ và một danh sách các thứ rồi báo cho chúng ta biết liệu thứ đó có là phần tử thuộc danh sách không. 
Nó thường được gọi là hàm trung tố vì nếu đọc theo kiểu đó sẽ dễ hơn.-}
elemFunction = 4 `elem` [3,4,5,6]


removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [ c | c <- st, c `elem` ['A'..'Z']]

{-def addOne(n) = [n] + addOne(n + 1)
-}
