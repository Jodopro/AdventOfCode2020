readInt x = read x::Int

file_numbers = ((fmap readInt) . lines) <$> readFile("InputDay1.txt")

mysplit' cur char [] = [cur]
mysplit' cur char (x:xs) | char==x   = cur:mysplit' [] char xs
                         | otherwise = mysplit' (cur ++ [x]) char xs

mysplit char xs = mysplit' [] char xs

find_combo' x [] = []
find_combo' x (y:ys)  | x+y == 2020 = [x*y]
                      | otherwise = find_combo' x ys


find_combo [] = []
find_combo (x:xs) = (find_combo' x xs) ++ find_combo xs


find_triple'' x y [] = []
find_triple'' x y (z:zs)  | x+y+z == 2020 = [x*y*z]
                          | otherwise = find_triple'' x y zs

find_triple' x [] = []
find_triple' x (y:ys) = (find_triple'' x y ys) ++ find_triple' x ys

find_triple [] = []
find_triple (x:xs) = (find_triple' x xs) ++ find_triple xs

part1 = find_combo <$> file_numbers

part2 = find_triple <$> file_numbers