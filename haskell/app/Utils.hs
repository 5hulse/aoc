module Utils
    ( getInputFile
    , getInputTestFile
    , loadInput
    , loadInputLines
    , resultString
    )
where

import Text.Printf
import System.IO

inputFileTmpl :: String
inputFileTmpl = "../inputs/Y%02dD%02d.txt"

inputTestFileTmpl :: String
inputTestFileTmpl = "../inputs/Y%02dD%02d-test.txt"

resultStringTmpl :: String
resultStringTmpl = "year %d, day %02d, part 1: %s\nyear %d, day %02d, part 2: %s"

resultString :: (Show a) => Int -> Int -> a -> a -> String
resultString year day part1 part2 = printf resultStringTmpl year day (show part1) year day (show part2)

getInputFile :: Int -> Int -> String
getInputFile year day = printf inputFileTmpl year day

getInputTestFile :: Int -> Int -> String
getInputTestFile year day = printf inputTestFileTmpl year day

loadInput :: String -> IO String
loadInput fileName =
    withFile fileName ReadMode $ \ handle -> do
        hGetContents handle

loadInputLines :: String -> IO [String]
loadInputLines fileName = do
    content <- loadInput fileName
    return (lines content)
