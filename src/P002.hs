fib :: [Int]
fib = fib' 0
  where
    fib' n | n < 2 = 1 : (fib' (n+1))
           | otherwise = let x = (fib !! (n - 1)) + (fib !! (n - 2)) in
                          if (x >= 4000000) then [] else x:fib' (n+1)


main = print $ sum (filter (\x -> x `mod` 2 == 0) fib)
