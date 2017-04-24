-- Theorem: if n + 2n == n xor 2n then X(n,2n,3n) == 0.
-- Claim: if n + 2n /= n xor 2n the X(n,2n,3n) /= 0.
-- proof: it means that 3n /= (n xor 2n). it means that 3n xor (n xor 2n) /= 0.

-- Theorem: iff some 2 adjuscent bits of n are 1, then n + 2n /= n xor 2n.
--

combination n r | r == 0 || r == n = 1
                | n < r = 0
                | otherwise = n * combination (n-1) (r-1) `div` r

numOfPossibleN numOf1 = combination (30 - numOf1 + 1) numOf1
main = print $ sum [numOfPossibleN i | i <- [0..30]]
