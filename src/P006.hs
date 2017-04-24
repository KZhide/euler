sumOfSquares n = sum (map (\i -> i * i) [1..n])
squareOfSum n = sum [1..n] ^ 2

main = print $ squareOfSum 100 - sumOfSquares 100
