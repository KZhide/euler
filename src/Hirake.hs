rotateL n = (++) <$> drop n <*> take n
indexes l = [0..length l - 1]
cycles s = fmap (`rotateL` s) (indexes s)

main = print $ cycles "abcdefg"
