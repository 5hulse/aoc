module Y2023D01 ( year2023day01 ) where

import qualified Utils

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import Text.Printf (printf)

filterString :: String -> String
filterString = filter (\c -> isDigit c)

replaceAlphaNumbers :: String -> String
replaceAlphaNumbers [] = []
replaceAlphaNumbers str@(x:xs)
    -- drop one letter fewer than the length of the matching string to account
    -- for overlaps like "oneeight", "threeight" etc.
    | isPrefixOf "one"   str   = ('1' : (replaceAlphaNumbers $ drop 2 str))
    | isPrefixOf "two"   str   = ('2' : (replaceAlphaNumbers $ drop 2 str))
    | isPrefixOf "three" str   = ('3' : (replaceAlphaNumbers $ drop 4 str))
    | isPrefixOf "four"  str   = ('4' : (replaceAlphaNumbers $ drop 3 str))
    | isPrefixOf "five"  str   = ('5' : (replaceAlphaNumbers $ drop 3 str))
    | isPrefixOf "six"   str   = ('6' : (replaceAlphaNumbers $ drop 2 str))
    | isPrefixOf "seven" str   = ('7' : (replaceAlphaNumbers $ drop 4 str))
    | isPrefixOf "eight" str   = ('8' : (replaceAlphaNumbers $ drop 4 str))
    | isPrefixOf "nine"  str   = ('9' : (replaceAlphaNumbers $ drop 3 str))
    | otherwise                = (x : replaceAlphaNumbers xs)

decodeLine :: String -> Int
decodeLine str = 10 * (digitToInt (head fstr)) + (digitToInt (last fstr))
    where
        fstr = filterString str

year2023day01 :: IO ()
year2023day01 = do
    list <- lines <$> (readFile $ Utils.getInputFile 2023 1)
    let
        part1 = sum [decodeLine str | str <- list]
        part2 = sum [decodeLine $ replaceAlphaNumbers str | str <- list]

    putStrLn $ printf "Part 1 Result: %d\nPart 2 Result: %d" part1 part2
