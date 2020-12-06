import Data.List.Split
import Data.List

-- easyReplace '\n' = ' '
-- easyReplace x = x

-- readPassports xs = map (splitOn ":") (splitOn " " (map easyReplace xs))

file_answers = ((fmap $ splitOn "\n") . (splitOn "\n\n")) <$> readFile("InputDay6.txt")

getCount1 answers = length $ nub $ foldr (++) [] answers

getCount2 (x:answers) = length $ filter (\x -> x) $ (foldr (&&) True) <$> (\y -> (elem y) <$> answers) <$> x

part1 = (sum . (fmap $ getCount1)) <$> file_answers

part2 = (sum . (fmap $ getCount2)) <$> file_answers