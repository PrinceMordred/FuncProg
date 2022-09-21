{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Main where

{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

import Data.Char
import Data.List
import Data.Maybe
import Text.XHtml (col, table)

-- | Model

type Field = String
type Row   = [Field]
type Table = [Row]

-- | Main

main :: IO ()
main = interact (unlines . exercise . lines)

exercise :: [String] -> [String]
exercise = printTable
         . project ["last", "first", "salary"]
         . select "gender" "male"
         . parseTable

-- | Parsing

-- * Exercise 1

parseTable :: [String] -> Table
parseTable = map words

-- | Printing

-- * Exercise 2

printLine :: [Int] -> String
printLine []     = "+"                                                    --at the end there needs to be a plus
printLine (x:xs) = "+" ++ replicate x '-' ++ printLine xs                 -- add x*"-"
-- * Exercise 3

printField :: Int -> String -> String
printField n s | all isDigit s = replicate (n - length s) ' ' ++ s        -- all numbers?
               | otherwise     = s ++ replicate (n - length s) ' '        -- elsecase
-- * Exercise 4

printRow :: [(Int, String)] -> String
printRow []     = "|"
printRow (x:xs) = "|" ++ uncurry printField x ++ printRow xs

-- * Exercise 5

columnWidths :: Table -> [Int]
columnWidths t = map (maximum . map length) (transpose t)                  -- get the max in each columns      

-- * Exercise 6

printTable :: Table -> [String]
printTable table@(header:rows) = [printLine lines,
                                map toUpper (printRow (zip lines header)),
                                printLine lines]
                                ++ printRows lines rows ++
                                 [printLine lines]
    where lines     = columnWidths table

printRows :: [Int] -> [Row] -> [String]
printRows l = map (printRow . zip l)
-- | Querying
-- * Exercise 7

select :: Field -> Field -> Table -> Table
select column value table@(header:rows) =
    case elemIndex column header of
        Nothing -> table
        Just columnIndex -> header : filter (predicate columnIndex) rows
    where predicate i rs = rs !! i == value 


-- * Exercise 8

project :: [Field] -> Table -> Table
project columns table@(header:_) = 
    transpose (map (transpose table !!) (mapMaybe (`elemIndex` header)columns))
