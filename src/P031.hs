f [] v | v == 0 = 1
       | otherwise = 0

f (c:cs) v | v == 0 = 1
           | otherwise = sum (map (\n -> f cs (v-c*n)) [0..(div v c)])

main = print $ f [200, 100, 50, 20, 10, 5, 2, 1] 200
