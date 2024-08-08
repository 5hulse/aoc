module Y2023D03 ( year2023day03 ) where

import qualified Utils

import Data.Char (isDigit)

year :: Int
year = 2023
day :: Int
day = 3

type X = Int
type Y = Int
type Nrows = Int
type Coord = (X, Y)
typeGrid
type SymbolLocs = [Coord]

findSymbols :: Nrows -> String -> SymbolLocs
findSymbols _ []  = []
findSymbols nrows str = map (idxToCoord nrows . fst) (filter checkSymbol (zip [0..] str))
    where
        checkSymbol = (\symb -> (not $ isDigit symb) && (symb /= '.')) . snd

idxToCoord :: Nrows -> Int -> Coord
idxToCoord nrows idx = (idx `div` nrows, idx `mod` nrows)

(+) :: Coord -> Coord -> Coord
(+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

relativePositions :: [Coord]
relativePositions = [ ((-1), (-1))
                    , ((-1), 0)
                    , ((-1), 1)
                    , (0, (-1))
                    , (0, 1)
                    , (1, (-1))
                    , (1, 0)
                    , (1, 1)
                    ]

retrieveNumbers :: SymbolLocs -> Grid


year2023day03 :: IO ()
year2023day03 = do
    lns <- lines <$> readFile (Utils.getInputFile year day)

    let
        nRows = length lns
        grid = concat lns
        symbols = findSymbols nRows grid

    let
        -- part1 :: Int
        part1 :: String
        part1 = "todo"

        -- part2 :: Int
        part2 :: String
        part2 = "todo"

    putStrLn $ Utils.resultString year day part1 part2
