ordinalDays = [31,31,28,31,30,31,30,31,31,30,31,30]
days month year | month == 2 && isLeap year = 29
                | otherwise = ordinalDays !! (month `mod` 12)
  where
    isLeap y = y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)


dayOfLastDate m y | (m,y) == (0, 1899) = 0
                  | m == 1 = dayOfLastDate 0 (y - 1) + days m y
                  | otherwise = dayOfLastDate (mod (m + 11) 12) y + days m y

dayOfFirstDate m y = dayOfLastDate m y - days m y + 1

main = print $ length $ filter (\(m,y) -> dayOfFirstDate m y `mod` 7 == 0) [(m,y)|m <- [0..11], y <- [1901..2000]]
