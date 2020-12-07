import Data.List.Split
import Data.List

data BagAmount = BagAmount String Int deriving Show

data Rule = Rule String [BagAmount] deriving Show

addWithSpace s1 s2 = s1 ++ ' ':s2

getBagAmount ba = BagAmount s n2
                where 
                    (n:rest) = splitOn " " ba
                    s = init $ foldr addWithSpace "" rest
                    n2 = read n::Int
                

readRule r = Rule rs bas3
            where 
                (rs:bas1:[]) = splitOn " bags contain " r
                bas2 = (head . (splitOn " bag")) <$> (splitOn ", " bas1)
                bas3 = if bas2!!0=="no other" then [] else fmap getBagAmount bas2

rules = ((fmap readRule) . lines) <$> readFile "InputDay7.txt"

isPossible' cur (BagAmount s i) = s==cur

isPossible cur (Rule s bas) = (length $ filter (isPossible' cur) bas) >= 1

getS :: Rule -> String
getS (Rule s bas) = s

getPossiblesFromCur :: String -> [Rule] -> [String]
getPossiblesFromCur cur rules = getS <$> filter (isPossible cur) rules

findPossibles [] updated rules = updated
findPossibles (cur:toUpdate) updated rules = findPossibles newToUpdate newUpdated rules
                                            where
                                                pfromc = getPossiblesFromCur cur rules
                                                newUpdated = cur:updated
                                                newToUpdate = toUpdate ++ (filter (\x -> not $ elem x (toUpdate ++ newUpdated)) pfromc)

part1 = ((\x -> x-1) . length . (findPossibles ["shiny gold"] [])) <$> rules

findRuleBas s1 ((Rule s2 bas):rules) | s1==s2    = bas 
                                     | otherwise = findRuleBas s1 rules

getSum s rules = 1 + (sum $ (\(BagAmount s2 i) -> i * (getSum s2 rules)) <$> bas)
                where
                    bas = findRuleBas s rules

part2 = ((\x -> x-1) . (getSum "shiny gold")) <$> rules