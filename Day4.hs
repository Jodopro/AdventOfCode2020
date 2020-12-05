import Data.List.Split

easyReplace '\n' = ' '
easyReplace x = x

readPassports xs = map (splitOn ":") (splitOn " " (map easyReplace xs))
            -- where [p1,r1] = splitOn "-" xs
            --       [r2,p4] = splitOn ": " r1
            --       [p2,p3] = splitOn " " r2

file_passwords = ((fmap readPassports) . (splitOn "\n\n")) <$> readFile("InputDay4.txt")

countCorrect extraCheck [] = 0
countCorrect extraCheck (x:xs) | isCorrect extraCheck x = 1 + countCorrect extraCheck xs
                               | otherwise              = countCorrect extraCheck xs

isCorrect extraCheck x = (containsByr extraCheck x) && (containsIyr extraCheck x) && (containsEyr extraCheck x) && (containsHgt extraCheck x) && (containsHcl extraCheck x) && (containsEcl extraCheck x) && (containsPid extraCheck x)

containsByr extraCheck [] = False
containsByr extraCheck ([x,y]:xs) | x=="byr"  = (not extraCheck) || (isNumber y && year >= 1920 && year <= 2002 && (length y) == 4)
                                  | otherwise = containsByr extraCheck xs
                                  where year = read y::Int

containsIyr extraCheck [] = False
containsIyr extraCheck ([x,y]:xs) | x=="iyr"  = (not extraCheck) || (isNumber y && year >= 2010 && year <= 2020 && (length y) == 4)
                                  | otherwise = containsIyr extraCheck xs
                                  where year = read y::Int

containsEyr extraCheck [] = False
containsEyr extraCheck ([x,y]:xs) | x=="eyr"  = (not extraCheck) || (isNumber y && year >= 2020 && year <= 2030 && (length y) == 4)
                                  | otherwise = containsEyr extraCheck xs
                                  where year = read y::Int

containsHgt extraCheck [] = False
containsHgt extraCheck ([x,y]:xs) | x=="hgt"  = (not extraCheck) || (isHeight y)
                                  | otherwise = containsHgt extraCheck xs

containsEcl extraCheck [] = False
containsEcl extraCheck ([x,y]:xs) | x=="ecl"  = (not extraCheck) || (y=="amb" || y=="blu" || y=="brn" || y=="gry" || y=="grn" || y=="hzl" || y=="oth")
                                  | otherwise = containsEcl extraCheck xs

containsHcl extraCheck [] = False
containsHcl extraCheck ([x,y]:xs) | x=="hcl"  = (not extraCheck) || (isHex y)
                                  | otherwise = containsHcl extraCheck xs

containsPid extraCheck [] = False
containsPid extraCheck ([x,y]:xs) | x=="pid"  = (not extraCheck) || ((length y)==9 && isNumber y)
                                  | otherwise = containsPid extraCheck xs

isDigit c = c >= '0' && c <='9'
isatof c = c >= 'a' && c <= 'f'

isNumber [] = True
isNumber (x:xs) = isDigit x && isNumber xs

isHex [] = False
isHex (x:xs) = x=='#' && length xs == 6 && isHex' xs

isHex' [] = True
isHex' (x:xs) = (isDigit x || isatof x) && isHex' xs

last2 xs = (last (init xs)):last xs:[]

isHeight xs | last2 xs == "cm" = isNumber n && n' >= 150 && n' <= 193
            | last2 xs == "in" = isNumber n && n' >= 59 && n' <= 76
            | otherwise = False
            where n = init (init xs)
                  n' = read n::Int

part1 = (countCorrect False) <$> file_passwords

part2 = (countCorrect True) <$> file_passwords