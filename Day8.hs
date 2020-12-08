import Data.List.Split

type Command = (String,Int)

readInts ('+':val) = read val::Int
readInts ('-':val) = -1 * read val::Int

runSingleCommand accumulator ("acc",n) commandIndex commands runnedCommands = run (accumulator + n) (commandIndex + 1) commands (commandIndex:runnedCommands)
runSingleCommand accumulator ("jmp",n) commandIndex commands runnedCommands = run accumulator (commandIndex + n) commands (commandIndex:runnedCommands)
runSingleCommand accumulator ("nop",n) commandIndex commands runnedCommands = run accumulator (commandIndex + 1) commands (commandIndex:runnedCommands)

run accumulator commandIndex commands runnedCommands | commandIndex >= length commands = ("finished", accumulator, length runnedCommands)
                                                     | elem commandIndex runnedCommands = ("infinite loop", accumulator, length runnedCommands)
                                                     | otherwise = runSingleCommand accumulator (commands!!commandIndex) commandIndex commands runnedCommands

parse commands = fmap (\(x:y:[]) -> (x,readInts y)) ((splitOn " ") <$> commands)

part1 = ((\x -> run 0 0 (parse x) []) . lines) <$> readFile "InputDay8.txt"

alterdParsings :: [Command] -> [Command] -> [[Command]] -> [[Command]]
alterdParsings passedCommands [] parsings = parsings
alterdParsings passedCommands (("acc",n):commands) parsings = alterdParsings (passedCommands ++ [("acc",n)]) commands parsings
alterdParsings passedCommands (("jmp",n):commands) parsings = alterdParsings (passedCommands ++ [("jmp",n)]) commands ((passedCommands ++ (("nop",n):commands)):parsings)
alterdParsings passedCommands (("nop",n):commands) parsings = alterdParsings (passedCommands ++ [("nop",n)]) commands ((passedCommands ++ (("jmp",n):commands)):parsings)

runAlterdParsings (p:parsings) | s == "finished" = a
                               | otherwise = runAlterdParsings parsings
                            where
                                (s,a,l) = run 0 0 p []

part2 = (runAlterdParsings . (\x -> alterdParsings [] (parse x) []) . lines) <$> readFile "InputDay8.txt"