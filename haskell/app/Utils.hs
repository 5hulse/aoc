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
resultStringTmpl = "year %d, day %02d:\nPart 1: %s\nPart 2: %s\n"

resultString :: (Show a, Show b) => Int -> Int -> a -> b -> String
resultString year day part1 part2 = printf resultStringTmpl year day (show part1) (show part2)

getInputFile :: Int -> Int -> String
getInputFile = printf inputFileTmpl

getInputTestFile :: Int -> Int -> String
getInputTestFile = printf inputTestFileTmpl

loadInput :: String -> IO String
loadInput fileName =
    withFile fileName ReadMode $ \ handle -> do
        hGetContents handle

loadInputLines :: String -> IO [String]
loadInputLines fileName = do
    content <- loadInput fileName
    return (lines content)
