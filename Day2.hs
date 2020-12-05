import Data.List.Split

readPassword xs = (read p1::Int,read p2::Int,head p3,p4)
            where [p1,r1] = splitOn "-" xs
                  [r2,p4] = splitOn ": " r1
                  [p2,p3] = splitOn " " r2

file_passwords = ((fmap readPassword) . lines) <$> readFile("InputDay2.txt")

isCorrect (min,max,c,s) = (fromIntegral min) <= count && (fromIntegral max) >= count
                        where count = length(filter (c==) s)

countIncorrect [] = 0
countIncorrect (x:xs) | isCorrect x = 1 + countIncorrect xs
                      | otherwise   = countIncorrect xs

isCorrect2 (pos1,pos2,c,s) = (b1 && (not b2)) || (b2 && (not b1))
                        where p1 = (fromIntegral pos1) - 1
                              p2 = (fromIntegral pos2) - 1
                              b1 = p1 < length s && c == s !! p1
                              b2 = p2 < length s && c == s !! p2

countIncorrect2 [] = 0
countIncorrect2 (x:xs) | isCorrect2 x = 1 + countIncorrect2 xs
                       | otherwise   = countIncorrect2 xs

part1 = countIncorrect <$> file_passwords

part2 = countIncorrect2 <$> file_passwords