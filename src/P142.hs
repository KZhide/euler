import Data.List

-- a2 = x + y
-- b2 = x - y
-- c2 = x + z
-- d2 = x - z
-- e2 = y + z
-- f2 = y - z
-- a2 > c2 > e2
-- a2 > c2 > b2
-- d2 > b2
-- d2 > f2
--
-- x = ave a2 b2
-- y = ave a2 -b2
-- z = c2-x


cand = [((a^2+b^2)`div`2, (a^2-b^2)`div`2, c^2 - (a^2+b^2)`div`2)| a<-[1..], c<-[1..a-1], b<-[1..c-1], (a+b)`mod`2 == 0, c^2-(a^2+b^2)`div`2 > 0]
intSqrt :: Integer -> Integer
intSqrt = floor.sqrt.fromIntegral
perfectSquare n = n == (intSqrt n)^2

perfectTriple (x, y, z) = all perfectSquare [x+y,x-y,x+z,x-z,y+z,y-z]



main = print $ find perfectTriple cand
