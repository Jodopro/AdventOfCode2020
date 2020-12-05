import Data.List

getId current [] = current
getId current (x:xs) | x == 'F' || x == 'L' = getId (current * 2) xs
                     | otherwise            = getId (current * 2 + 1) xs

part1 = (maximum . (fmap (getId 0)) . lines) <$> readFile "InputDay5.txt"

findMissing (x:[]) = x + 1
findMissing (x:y:xs) | y == x+1  = findMissing (y:xs)
                     | otherwise = x + 1

part2 = (findMissing . sort . (fmap (getId 0)) . lines) <$> readFile "InputDay5.txt"