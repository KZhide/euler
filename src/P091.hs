nVec (x,y) = (y `div` gcd x y, -x `div` gcd x y)
vAdd (x1, y1) (x2, y2) = (x1+x2, y1+y2)

valid (x, y) = 0 <= x && x <= 50 && 0 <= y && y <= 50

