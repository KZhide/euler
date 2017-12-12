import Control.Monad.State

test = do
  a <- get
  put $ a+1
  modify (*2)
  return a

main = do
  print $ runState test 5
