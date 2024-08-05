module Utils
    ( getInputFile
    , getInputTestFile
    , loadInput
    , loadInputLines
    )
where

import Text.Printf
import System.IO

inputFileTmpl :: String
inputFileTmpl = "../inputs/Y%02dD%02d.txt"

inputTestFileTmpl :: String
inputTestFileTmpl = "../inputs/Y%02dD%02d-test.txt"

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
