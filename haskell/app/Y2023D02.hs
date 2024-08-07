module Y2023D02 ( year2023day02 ) where

import qualified Utils

import Data.Char (isDigit)

year :: Int
year = 2023

day :: Int
day = 2

data GameLimits
    = GameLimits { red :: Int
                 , blue :: Int
                 , green :: Int
                 } deriving (Show)

partOneLimits :: GameLimits
partOneLimits
    = GameLimits { red = 12
                 , blue = 14
                 , green = 13
                 }

data MaxColors
    = MaxColors { maxRed :: Int
                , maxBlue :: Int
                , maxGreen :: Int
                } deriving (Show)

power :: MaxColors -> Int
power MaxColors { maxRed = r
                , maxBlue = b
                , maxGreen = g
                }
    = r * b * g

validGame :: GameLimits -> MaxColors -> Bool
validGame
    ( GameLimits { red = r
                 , blue = b
                 , green = g
                 }
    )
    ( MaxColors { maxRed = maxr
                , maxBlue = maxb
                , maxGreen = maxg
                }
    )
    =
        maxr <= r && maxb <= b && maxg <= g

-- Takes list of strings like:
--     "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
--
-- and returns a list of results of the form
--     (4, MaxColors { maxRed = 14, maxBlue = 15, maxGreen = 3 })
--
-- Where the first element is the game number
--       the second element is the maximum number of cubes of each color drawn
parse :: [String] -> [(Int, MaxColors)]
parse []     = []
parse (x:xs) = (number, maxCols) : parse xs
    where
        (number, rmGame) = fetchNumber x
        instanceStrs = splitAtChar ';' rmGame
        -- Use of `tail` is to remove preceeding whitepace char
        splitStrs = map (map tail . splitAtChar ',') instanceStrs
        maxCols = MaxColors { maxRed = getMaxColor "red" splitStrs
                            , maxBlue = getMaxColor "blue" splitStrs
                            , maxGreen = getMaxColor "green" splitStrs
                            }

-- Takes a String of the form
--     "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
-- And returns a tuple of the form
--     (4, " 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")
-- Where the first element is the game number
--       the second element is the String with the "Game" title removed
fetchNumber :: String -> (Int, String)
fetchNumber str = (read numStr :: Int, rest)
    where
        -- Get rid of "Game "
        rmGame = drop 5 str
        numStr = takeWhile isDigit rmGame
        -- Get rid of ":"
        rest = drop (length numStr + 1) rmGame

-- Split String at every occurrence of the specified Char
splitAtChar :: Char -> String -> [String]
splitAtChar _ "" = []
splitAtChar c str = first : splitAtChar c rest
    where
        first = takeWhile (/=c) str
        rest = drop (length first + 1) str

-- Takes arguments of the form
-- 1.     "red" "20 red"
-- 2.     "blue" "20 red"
-- For case 1. 20 will be returned
-- For case 2. 0 will be returned
fetchNColor :: String -> String -> Int
fetchNColor color str =
    if lastN lenColor str == color
    then
        read (take lenNum str) :: Int
    else
        0

    where
        lenColor = length color
        lenNum = length str - lenColor - 1

-- Takes a String (i.e "red", "blue" or "green")
-- and a list of lists like
--     str = [["1 green", "3 red", "6 blue"], ["3 green", "6 red"], ["3 green", "15 blue", "14 red"]]
-- and returns the maximum value corresponding to the color specified:
--     getMaxColor "blue" str = 15
--     getMaxColor "red" str = 14
--     getMaxColor "green" str = 3
getMaxColor :: String -> [[String]] -> Int
getMaxColor color strs = maximum $ map (sum . map (fetchNColor color)) strs

-- Recover the last n elements of a list
lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

year2023day02 :: IO ()
year2023day02 = do
    list <- lines <$> readFile (Utils.getInputFile year day)
    let
        info = parse list
        part1 = sum $ map fst $ filter (validGame partOneLimits . snd) info
        part2 = sum $ map (power . snd) info

    putStrLn $ Utils.resultString year day part1 part2
