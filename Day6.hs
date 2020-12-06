import Data.List.Split
import Data.List

part1 = (sum . (fmap (\answers -> length $ nub $ foldr (++) [] answers)) . (fmap $ splitOn "\n") . (splitOn "\n\n")) <$> readFile("InputDay6.txt")

part2 = (sum . (fmap (\(x:answers) -> length $ filter (\x -> x) $ (foldr (&&) True) <$> (\y -> (elem y) <$> answers) <$> x)) . (fmap $ splitOn "\n") . (splitOn "\n\n")) <$> readFile("InputDay6.txt")