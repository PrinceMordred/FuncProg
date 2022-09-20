module Main where
import Data.List (intercalate)
main = interact ( intercalate " / ". map reverse . lines) 