data Pos = Pos Integer Integer Integer
         deriving (Eq, Show)

top (Pos r 0 i) = Pos (r+1) 0 i
top (Pos r 1 0) = Pos (r+1) 0 r
top (Pos r 1 i) = Pos r 1 (i-1)
top (Pos r 2 0) = Pos r 1 r
top (Pos r 2 i) = Pos (r-1) 2 (i-1)
top (Pos r 3 i) | i == r = Pos (r-1) 4 0
                | otherwise = Pos (r-1) 3 i
top (Pos r )


ringSize 0 = 1
ringSize n = 6 * n

cornerTop 0 = 1
cornerTop n = cornerTop (n-1) + ringSize (n-1)


