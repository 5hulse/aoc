{-# LANGUAGE FlexibleContexts #-}

module Y2023D02 ( year2023day02 ) where

import qualified Utils

import Text.Regex.TDFA

year :: Int
year = 2023
day :: Int
day = 2

data Cube = Red Int | Blue Int | Green Int deriving (Show, Read)

type Input = [[Cube]]

parse :: [String] -> Input
parse = map (map procCube . matchCubeStr)
    where matchCubeStr str = str =~ "([0-9]+) (red|green|blue)" :: [[String]]
          procCube [_, n, "red"]   = Red   (read n)
          procCube [_, n, "blue"]  = Blue  (read n)
          procCube [_, n, "green"] = Green (read n)
          procCube _               = error "Shouldn't get here!"

year2023day02 :: IO ()
year2023day02 = do
    lns <- lines <$> readFile (Utils.getInputFile year day)
    let
        input = parse lns

        part1 :: Int
        part1 = sum $ map fst $ filter (all validGame . snd) (zip [1..] input)
            where validGame (Red n)   = n <= 12
                  validGame (Green n) = n <= 13
                  validGame (Blue n)  = n <= 14

        part2 :: Int
        part2 = sum $ map (\x -> maxRed x * maxGreen x * maxBlue x) input
            where
                maxRed     = maximum . map (\(Red r) -> r) . filter isRed
                maxBlue    = maximum . map (\(Blue r) -> r) . filter isBlue
                maxGreen   = maximum . map (\(Green r) -> r) . filter isGreen
                isRed (Red _)     = True
                isRed _           = False
                isBlue (Blue _)   = True
                isBlue _          = False
                isGreen (Green _) = True
                isGreen _         = False

    putStrLn $ Utils.resultString year day part1 part2
