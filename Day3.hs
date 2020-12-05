readForrest [] = []
readForrest ('.':xs) = 0:readForrest xs
readForrest ('#':xs) = 1:readForrest xs
readForrest (x:xs)   = 0:readForrest xs

file_forrest = ((fmap readForrest) . lines) <$> readFile("InputDay3.txt")

getTrees index slope [] = 0
getTrees index slope (x:xs) = (x !! index) + getTrees (mod (index + slope) 31) slope xs

getTrees2 index slope [] = 0
getTrees2 index slope (x:[]) = 0
getTrees2 index slope (x:y:xs) = (x !! index) + getTrees2 (mod (index + slope) 31) slope xs

part1 = (getTrees 0 3) <$> file_forrest

p2getTrees forrest = x1*x2*x3*x4*x5
                   where x1 = getTrees 0 1 forrest
                         x2 = getTrees 0 3 forrest
                         x3 = getTrees 0 5 forrest
                         x4 = getTrees 0 7 forrest
                         x5 = getTrees2 0 1 forrest

part2 = p2getTrees <$> file_forrest