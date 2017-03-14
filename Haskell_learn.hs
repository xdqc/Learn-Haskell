import Data.List
import System.IO

--some basic syntax
minT = minBound :: Int
mod1 = 34 `mod` 7
mylist = 2:3:5:7:[]
mylist2 = mylist ++ 11:[]
mylist3 = 1:mylist2
secondPrime = mylist !! 1
initPrime = init mylist2
is7inList = 7 `elem` mylist
to100 = [1..100]
evenL = [2,4..100]
letterL = ['A','C'..'z']
mulTable = [ [x*y | y <- [1..5]] | x <- [1..3] ]
card = ("K",'c')

--conditional functions
getListItems :: [Int]->String
getListItems [] = "empty list"
getListItems (x:[]) = "List start with: "++ show x
getListItems (x:y:[]) = "List contains: "++ show x ++ " and " ++ show y
getListItems (x:xs) = "The first is "++show x ++", the rest are " ++ show xs

getFirstLetter::String->String
getFirstLetter[] = "empty string"
getFirstLetter all@(x:xs) = "First in "++all++" is "++show x

