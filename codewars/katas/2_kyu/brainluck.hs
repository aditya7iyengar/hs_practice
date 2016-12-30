--Description:

--Inspired from real-world Brainf**k, we want to create an interpreter of that language which will support the following instructions (the machine memory or 'data' should behave like a potentially infinite array of bytes, initialized to 0):

--> increment the data pointer (to point to the next cell to the right).
--< decrement the data pointer (to point to the next cell to the left).
--+ increment (increase by one, truncate overflow: 255 + 1 = 0) the byte at the data pointer.
--- decrement (decrease by one, treat as unsigned byte: 0 - 1 = 255 ) the byte at the data pointer.
--. output the byte at the data pointer.
--, accept one byte of input, storing its value in the byte at the data pointer.
--[ if the byte at the data pointer is zero, then instead of moving the instruction pointer forward to the next command, jump it forward to the command after the matching ] command.
--] if the byte at the data pointer is nonzero, then instead of moving the instruction pointer forward to the next command, jump it back to the command after the matching [ command.
--The function will take in input...

--the program code, a string with the sequence of machine instructions,
--the program input, a string, eventually empty, that will be interpreted as an array of bytes using each character's ASCII code and will be consumed by the , instruction
--... and will return ...

--the output of the interpreted code (always as a string), produced by the . instruction.

-- MY SOLUTION
module Brainfuck
    ( executeString
    ) where
import qualified Data.Map as Map

executeString :: String -> String -> Maybe String
executeString = es Map.empty 0

es :: Map.Map Int Char -> Int -> String -> String -> Maybe String
es _ _ "" _ = Just ""
es dat pointer (input:source) inp
  | input == '>' = es dat (succ pointer) source inp
  | input == '<' = es dat (pred pointer) source inp
  | input == '+' = es (addAt pointer dat) pointer source inp
  | input == '-' = es (subAt pointer dat) pointer source inp
  | input == '.' = maybe Nothing (Just . (readAt pointer dat :)) $ es dat pointer source inp
  | input == ',' = maybe Nothing (\x -> es (writeAt pointer x dat) pointer source (tail inp)) $ readIn inp
  | input == '[' = let source' = if readAt pointer dat == '\0' then fromClosingBracket source else toClosingBracket source ++ (input:source)
                 in es dat pointer source' inp
  | input == ']' = es dat pointer source inp
  where
    addAt input dat = writeAt input (succ $ readAt input dat) dat
    subAt input dat = writeAt input (pred $ readAt input dat) dat
    readAt input dat = maybe '\0' id $ Map.lookup input dat
    writeAt input x dat = Map.insert input x dat
    readIn [] = Nothing
    readIn (input:_) = Just input
    fromClosingBracket s = snd $ findClosingBracket 0 s
    toClosingBracket s = take (fst $ findClosingBracket 0 s) s
    findClosingBracket input (c:s)
      | c == '[' = let (sub, rest) = findClosingBracket (input + 1) s in findClosingBracket sub rest
      | c == ']' = (input + 1, s)
      | otherwise = findClosingBracket (input + 1) s
