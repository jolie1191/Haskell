

import Data.List
import Test.QuickCheck

drop' :: [Int] -> [Int]
drop' [] = []
drop' [x] = [x]
drop' (x:y:xs) = x:drop' xs

drop2 :: [Int] -> [Int]
drop2 (x:xs) = filter odd (x:xs) 


sort' :: [Int] -> [Int]
sort' [] = []
sort' (x:xs) = minval:sort' (delete minval (x:xs))
               where
               minval = minimum (x:xs)

--Quick Check for testing the list sorted
-- return value should be Bool (1)
ordered :: [Int] -> Bool
ordered [] = True
ordered [x] = True
ordered (x:y:xs) = x <= y && ordered (y:xs)


prop_ordered :: [Int] -> Bool
prop_ordered xs = ordered $ sort' xs

--Use quickCheck prop_ordered to check random list 

-- (2) quickCheck 
sorted :: [Int] -> Bool
sorted xs = and $ zipWith (<=) xs (tail xs)


prop_sorted :: [Int] -> Bool
prop_sorted xs = sorted $ sort' xs

prop_sorted2 :: [Int] -> Bool
prop_sorted2 xs = if xs == [] then True
                  else if (length xs) == 1 then True
                  else if (minimum xs) == head (sort' xs) then True
                  else False
                  
prop_sorted3 :: [Int] -> Bool
prop_sorted3 xs = if (length xs) == length (sort' xs) then True
                  else False

prop_sorted4 :: [Int] -> Bool
prop_sorted4 xs = if (sum xs) == sum (sort' xs) then True
                  else False





-- instance of Show for type Day
data Day = Mon | Tue | Wed | Thu | Fri

instance Show Day where
         show Mon = "Mon"
         show Tue = "Tue"
         show Wed = "Thu"
         show Fri = "Fri"