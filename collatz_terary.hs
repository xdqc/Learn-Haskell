-- Base 10 num
collatz x =
    if mod x 2 == 0 
    then div x 2
    else x*3+1
-- generate a collatz sequence
clz x = takeWhile (\x -> x>1) $ iterate collatz x

clzStep x = (x,length$clz x)
clzSteps xs = map clzStep xs
select a xs = [ (x,y) | (x,y) <- xs, y > a ]


-- Base 3 (ternary)
dec2ter x = 
    if x > 0
    then mod x 3 + dec2ter(div x 3) * 10
    else 0
-- Print collatz secqence in ternary form line by line
prt xs = mapM_ print $ map dec2ter xs

