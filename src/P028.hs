bottomRight n = 2 * n + topRight (n - 1)
bottomLeft n = 2 * n + bottomRight n
topLeft n = 2 * n + bottomLeft n
topRight n = (2 * n + 1) ^ 2

main = print $ sum (fmap (\n -> bottomRight n + bottomLeft n + topLeft n + topRight n) [1..500]) + 1


