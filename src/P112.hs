import Digits
import Data.List (find, scanl')
import System.Environment (getArgs)
import Memorize

increasing [] = True
increasing [x] = True
increasing (x:y:ys) | x <= y = increasing (y:ys)
                    | otherwise = False

decreasing [] = True
decreasing [x] = True
decreasing (x:y:ys) | x >= y = decreasing (y:ys)
                    | otherwise = False

incNum = increasing . digits
decNum = decreasing . digits
bounceNum n = not (incNum n) && not (decNum n)

bounceRate n = (fromIntegral.length.filter bounceNum) [1..n] / fromIntegral n

f = scanl' g (0,0)
 where
   g (b,s) n = if bounceNum n then (b+1, s+1) else (b, s+1)

loggingFind :: (a -> IO ()) -> (a -> IO Bool) -> [a] -> IO (Maybe a)
loggingFind log cond [] = return Nothing
loggingFind log cond (x:xs) = do
  log x
  b <- cond x
  if b then return (Just x) else loggingFind log cond xs

main = do
  print $ find (\(b,s) -> b*100==s*99) $ tail $ f [1..]
  --args <- getArgs
  --a <- loggingFind (\n -> print ( (show n) ++ " " ++ show (bounceRate n))) (return.(>0.99).bounceRate) [(read (head args) :: Integer)..]
  --print a
