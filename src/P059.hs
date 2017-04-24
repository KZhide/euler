import Data.List.Split
import Data.List (group,sortOn,sort)
import Data.Bits

l0 [] = []
l0 (x:xs) = x:(l2 xs)
l1 [] = []
l1 (x:xs) = l0 xs
l2 [] = []
l2 (x:xs) = l1 xs

decG [] = []
decG (x:xs) = (xor (fromEnum 'g') x):(decO xs)
decO [] = []
decO (x:xs) = xor (fromEnum 'o') x : decD xs
decD [] = []
decD (x:xs) = xor (fromEnum 'd') x : decG xs

main = do
  content <- readFile "p059_cipher.txt"
  let letters = fmap (read::String -> Int) (splitOn "," content) in
    print $ sum (decG letters)
  --print $ xor 71 101
