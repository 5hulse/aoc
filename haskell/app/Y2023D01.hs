module Y2023D01 ( year2023day01 ) where

import qualified Utils

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)

filterString :: String -> String
filterString = filter isDigit

replaceAlphaNumbers :: String -> String
replaceAlphaNumbers [] = []
replaceAlphaNumbers str@(x:xs)
    -- drop one letter fewer than the length of the matching string to account
    -- for overlaps like "oneeight", "threeight" etc.
    | "one"   `isPrefixOf` str = '1' : replaceAlphaNumbers (drop 2 str)
    | "two"   `isPrefixOf` str = '2' : replaceAlphaNumbers (drop 2 str)
    | "three" `isPrefixOf` str = '3' : replaceAlphaNumbers (drop 4 str)
    | "four"  `isPrefixOf` str = '4' : replaceAlphaNumbers (drop 3 str)
    | "five"  `isPrefixOf` str = '5' : replaceAlphaNumbers (drop 3 str)
    | "six"   `isPrefixOf` str = '6' : replaceAlphaNumbers (drop 2 str)
    | "seven" `isPrefixOf` str = '7' : replaceAlphaNumbers (drop 4 str)
    | "eight" `isPrefixOf` str = '8' : replaceAlphaNumbers (drop 4 str)
    | "nine"  `isPrefixOf` str = '9' : replaceAlphaNumbers (drop 3 str)
    | otherwise                =  x  : replaceAlphaNumbers xs

decodeLine :: String -> Int
decodeLine str = 10 * digitToInt (head fstr) + digitToInt (last fstr)
    where
        fstr = filterString str

year2023day01 :: IO ()
year2023day01 = do
    list <- lines <$> readFile (Utils.getInputFile 2023 1)
    let
        part1 = sum [decodeLine str | str <- list]
        part2 = sum [decodeLine $ replaceAlphaNumbers str | str <- list]

    putStrLn $ Utils.resultString 2023 1 part1 part2
