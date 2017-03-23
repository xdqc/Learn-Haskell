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
andtest = and [all (\x -> mod x 2 == 0) [1..100], any (\x -> mod x 2 == 0) [1..100], elem 10 [1..]]
breaktest = break (\x -> x>5) [1..10]
spantest = span  (\x -> x<6) [1..10]
first10 = take 10 $cycle [1..7]
droploop = dropWhile (\x -> x<5) [3,1,4,9,2,6]
mulTable = [ [x*y | y <- [1..5]] | x <- [1..3] ]
card = ("K",'c') -- fst snd only works with 2 elem

--functions
addNum :: Int -> Int -> Int
addNum x y = x + y
isEven x = mod x 2 == 0

add2 = addNum 2
mul2 x = addNum x x
mul x 1 = x
mul x y = addNum x (mul x (y-1))
pow2 x = mul x x
pow x 1 = x
pow x y = mul x (pow x (y-1))   -- inefficient  O(2^n)

(a,b).+(c,d) = (addNum a c, addNum b d)

pass2 x = x 2
pass x y = y x
very x y = x (x y)
extremely x y = x (x (x y))
toBeVeryPowerful x = map (very (x**))
removeOdd = filter isEven
factorial x = foldl (*) 1 [1..x]
x3 x = map (\x -> mul2 x) [1..x]

--function operation (.)composition ($)application
-- f $ g x = f (g x)

collatz x =
    if isEven x 
    then div x 2
    else x*3+1
clz x = takeWhile (\x -> x>1) $ iterate collatz x
clzPair x = (x,length$clz x)
clzTrace x = map clzPair x

--conditional functions
getListItems :: [Int]->String
getListItems [] = "empty list"
getListItems (x:[]) = "List start with: "++ show x
getListItems (x:y:[]) = "List contains: "++ show x ++ " and " ++ show y
getListItems (x:xs) = "The first is "++show x ++", the rest are " ++ show xs

getFirstLetter::String->String
getFirstLetter[] = "empty string"
getFirstLetter all@(x:xs) = "First in "++all++" is "++show x

--type
type Point = (Double, Double)
midPoint :: Point -> Point -> Point
midPoint (x1,y1) (x2,y2) = ((x1+x2)/2, (y1+y2)/2)

type Idt = [Char]
newtype Id = Id { unId :: String }
id2str (Id i) = i

--record    not extensible nor shared field names
data Card' = Makecard 
    { value :: Int      -- value is not type, it's function
    , digit :: Id
    , suit :: Char
    }

kh = Makecard           -- Makecard :: Int -> Id -> Char -> Card'
    { value = 10        -- value :: Card' -> Int
    , digit = Id "K"    -- digit :: Card' -> String
    , suit = 'h'        -- suit  :: Card' -> Char
    }
qh = kh { digit = Id "Q"}

--Algebraic data type
data Suit = Spade|Heart|Diamond|Club      -- like enum
data Digit = Ace|Tw|Tr|Fo|Fv|Sx|Sv|Et|Ni|Tn|J|Q|K
data Card = Card Int Digit Suit        -- position is important
getDigit :: Card -> Digit
getDigit (Card a b c) = b              -- pattern matching
getValue (Card x ___ _) = x
kc = Card 10 K Club

data Folder = Folder String [Folder]

data MaybeInt = NoInt | JustInt Int     -- nullable Int type
data StringList = EmptyStrList | ConsStrList String StringList

--Parameterized Types (like generics)
data Maybe' a = Just' a | Nothing'
instance (Eq a) => Eq (Maybe' a) where
    Nothing' == Nothing' = True
    Nothing' == Just' _ = False
    Just' _ == Nothing' = False
    Just' x == Just' y = x==y


--Type class
elem' :: Eq a => a -> [a] -> Bool       -- add (Eq a) to the type signature for x==y
elem' _ [] = False
elem' x (y:ys)
    | x==y = True
    | otherwise = elem x ys

instance Eq Card where                  -- analog implementing Eq interface
    (Card v1 _ _)==(Card v2 _ _) = v1==v2
instance Ord Card where
    (Card v1 d1 s1) < (Card v2 d2 s2) = v1 < v2
    (Card v1 d1 s1) <= (Card v2 d2 s2) = v1 <= v2
instance Show Digit where
    show Ace = "A "
    show Tw = "2 "
    show Tr = "3 "
    show Fo = "4 "
    show Fv = "5 "
    show Sx = "6 "
    show Sv = "7 "
    show Et = "8 "
    show Ni = "9 "
    show Tn = "10"
    show J = "J "
    show Q = "Q "
    show K = "K "
instance Show Suit where
    show Club = ".c"
    show Heart = ".h"
    show Spade = ".s"
    show Diamond = ".d"
instance Show Card where
    show (Card _ d s) = " <" ++ show d ++ show s ++ "> "

cards = [Card 1 Ace Heart, Card 10 Tn Spade, Card 10 J Diamond]
qc = Card 10 Q Club                     -- elem qc cards  = True


--Defining Type class
class Distancable a where
    distance :: a -> a -> Double
pathLength :: Distancable a => [a] -> Double
pathLength [] = 0
pathLength (_:[]) = 0
pathLength (p0:p1:ps) = distance p0 p1 + pathLength (p1:ps)

class Valuable a where
    pts :: a -> Int
{-
instance Valuable Card where
    pts (Card _ Ace _) = 1
    pts (Card _ Tw _) = 2
    pts (Card _ Tr _) = 3
    pts (Card _ Fo _) = 4
    pts (Card _ Fv _) = 5
    pts (Card _ Sx _) = 6
    pts (Card _ Sv _) = 7
    pts (Card _ Et _) = 8
    pts (Card _ Ni _) = 9
    pts (Card _ Tn _) = 10
    pts (Card _ J _) = 10
    pts (Card _ Q _) = 10
    pts (Card _ K _) = 10
-}
data Hand = Hand [Card] BlackJack Bust
getCards (Hand a _ _) = a
instance Valuable Hand where
    pts (Hand [Card a _ _] _ _) = sum[a]

type BlackJack = Bool
blackJack :: Hand -> BlackJack
blackJack a = (pts a == 21) && (length (getCards a) == 2)

type Bust = Bool
bust :: Hand -> Bust
bust a = pts a > 21



