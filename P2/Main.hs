{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- A handy syntax extension. See:

    http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/syntax-extns.html#tuple-sections

-}

module Main where -- Rename to "Main" if you want to compile the game.
                         -- Don't forget to rename it back when submitting!

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import System.IO


-- | Rose trees

data Rose a = MkRose a [Rose a]
    deriving (Eq, Show)

-- Exercise 1
rose = MkRose 1 [MkRose 3 [], MkRose 4 [], MkRose 5 []]
rose2 = MkRose 12 [rose, rose, rose]

root :: Rose a -> a
root (MkRose r _) = r

children :: Rose a -> [Rose a]
children (MkRose _ c) = c

-- Exercise 2

size :: Rose a -> Int
size (MkRose _ []) = 1
size (MkRose r c) = succ . sum $ map size c

leaves :: Rose a -> Int
leaves (MkRose _ []) = 1
leaves (MkRose _ c) = sum $ map leaves c

-- | State representation

-- * Players

data Player = P1 | P2
    deriving (Eq, Ord)

instance Show Player where
    show P1 = "Player 1"
    show P2 = "Player 2"

-- Exercise 3

nextPlayer :: Player -> Player
nextPlayer p | p == P1   = P2
             | otherwise = P1

-- * Board

data Field = X | O | B
    deriving (Eq, Ord)

instance Show Field where
    show X = "X"
    show O = "O"
    show B = " "

-- Exercise 4

symbol :: Player -> Field
symbol p | p == P1  = X
         | otherwise = O

player :: Field -> Maybe Player
player X = Just P1
player O = Just P2
player B = Nothing

type Row   = (Field, Field, Field)
type Board = (Row, Row, Row)

tupToList :: (a,a,a) -> [a]
tupToList (x,y,z) = [x,y,z]

tup2ToList :: (a,a) -> [a]
tup2ToList (x,y) = [x,y]

listToTup :: [a]-> (a,a,a)
listToTup [x,y,z] = (x,y,z)

-- Exercise 5

verticals :: Board -> (Row, Row, Row)
verticals ((a,b,c), (d,e,f), (g,h,i)) = ((a,d,g), (b, e, h), (c, f, i))

diagonals :: Board -> (Row, Row)
diagonals ((a,b,c), (d,e,f), (g,h,i)) = ((a,e,i), (c, e, g))

-- Exercise 6

fullBoard = ((X,O,X), (O,X,O),(O,X,O))
emptyBoard = ((B,B,B),(B,B,B),(B,B,B))

-- Exercise 7

printBoard :: Board -> String
printBoard ((a,b,c), (d,e,f), (g,h,i)) = show a ++ "|" ++ show b ++ "|" ++ show c ++ "|" ++ "\n-+-+-\n" ++
                                         show d ++ "|" ++ show e ++ "|" ++ show f ++ "|" ++ "\n-+-+-\n" ++
                                         show g ++ "|" ++ show h ++ "|" ++ show i ++ "|"


-- | Move generation

-- Exercise 8

moves :: Player -> Board -> [Board]
moves p = traverseAll f
    where f  = traverseAll f2
          f2 B = [symbol p]
          f2 _ = []

traverseFst :: (a -> [d]) -> (a,b,c) -> [(d,b,c)]
traverseFst f (a,b,c) =  map (,b,c) $ f a

traverseSnd :: (b -> [d]) -> (a,b,c) -> [(a,d,c)]
traverseSnd f (a,b,c) =  map (a, ,c) $ f b

traverseTrd :: (c -> [d]) -> (a,b,c) -> [(a,b,d)]
traverseTrd f (a,b,c) =  map (a,b,) $ f c

traverseAll :: (a -> [a]) -> (a,a,a) -> [(a,a,a)]
traverseAll f t = traverseTrd f t <> traverseSnd f t <> traverseFst f t

-- | Gametree generation

-- Exercise 9
(%%) :: Maybe a -> Maybe a -> Maybe a
Nothing %% Nothing = Nothing
Just x %% _ = Just x
_ %% Just y = Just y


hasWinner :: Board -> Maybe Player
hasWinner b =   foldl (%%) Nothing (map (rowWinner . tupToList) (tupToList b) <>
                 map (rowWinner . tupToList) (tupToList $ verticals b) <>
                 map (rowWinner . tupToList) (tup2ToList $ diagonals b))

rowWinner :: [Field] -> Maybe Player
rowWinner [a,b,c] | a == b && b == c = player a
                  | otherwise = Nothing
-- Exercise 10
gameTree :: Player -> Board -> Rose Board
gameTree p b  = case hasWinner b of
  Just pl -> MkRose b []
  Nothing -> case elemIndex B (concatMap tupToList (tupToList b)) of
             Nothing -> MkRose b []
             Just pl -> MkRose b (map (gameTree (nextPlayer p)) (moves (nextPlayer p) b))



-- | Game complexity

-- Exercise 11

gameTreeComplexity :: Int
gameTreeComplexity = leaves $ gameTree P1 emptyBoard

-- | Minimax

-- Exercise 12

minimax :: Player -> Rose Board -> Rose Int
minimax p rb = case hasWinner $ root rb of
  Just pl -> MkRose (state $ root rb) []    --bottom level
  Nothing -> MkRose a (map b c)             --no winner = not bottom level
    where a = maximum $ map (root . b) c    -- take the max / min of all the roots of the rb
          b = minimax (nextPlayer p)
          c = children rb

--P1 win = 1 | tie =0 | P2 = -1

state :: Board -> Int
state b = case elemIndex B (concatMap tupToList (tupToList b)) of
        Nothing -> 0 --tie
        Just _  -> case hasWinner b of
          Nothing -> undefined -- this should never happen
          Just pl -> if pl == P1 then  1
                                 else -1



-- * Lazier minimum and maximums

-- Exercise 13

minimum' :: [Int] -> Int
minimum' = undefined

maximum' :: [Int] -> Int
maximum' = undefined

-- | Gameplay

-- Exercise 14

makeMove :: Player -> Board -> Maybe Board
makeMove = undefined

-- | Main

data PlayerType = Human | Computer

instance Show PlayerType where
    show Human    = "H"
    show Computer = "C"

main :: IO ()
main = do
    typeOfP1 <- askFor "Should Player 1 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]
    typeOfP2 <- askFor "Should Player 2 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]

    let playerType :: Player -> PlayerType
        playerType P1 = typeOfP1
        playerType P2 = typeOfP2

        gameLoop :: Player -> Board -> IO ()
        gameLoop p b = do
            putStrLn ("\n" ++ printBoard b)
            case hasWinner b of
                Just p  -> putStrLn (show p ++ " has won!")
                Nothing -> do
                    putStr   ("It's " ++ show p ++ "'s turn. ")
                    mb' <- case playerType p of
                        Human    -> humanMove    p b
                        Computer -> computerMove p b
                    case mb' of
                        Nothing -> do putStr   "No more moves are possible. "
                                      putStrLn "It's a draw."
                        Just b' -> gameLoop (nextPlayer p) b'

        humanMove :: Player -> Board -> IO (Maybe Board)
        humanMove p b =
            case moves p b of
              [] -> return Nothing
              possibleMoves -> do
                putStrLn "Possible moves are:"
                putStrLn (listMoves possibleMoves)
                i <- askFor "Make your choice:" [1..length possibleMoves]
                return (Just (possibleMoves !! (i-1)))

        computerMove :: Player -> Board -> IO (Maybe Board)
        computerMove p b = do
            putStrLn "Thinking..."
            return (makeMove p b)

        listMoves :: [Board] -> String
        listMoves = intercalate "\n"
                    . map (intercalate "    ")
                    . transpose
                    . map lines
                    . map (\(i,b) -> "(" ++ show i ++ "): \n" ++ printBoard b)
                    . zip [1 :: Integer ..]

    gameLoop P1 emptyBoard

askFor :: Show a => String -> [a] -> IO a
askFor m xs = do
    putStr (m ++ " ")
    hFlush stdout
    i <- getLine
    case find ((map toLower i ==) . map toLower . show) xs of
        Nothing -> do putStrLn $ "I didn't understand you. Enter one of: "
                                 ++ intercalate ", " (map show xs) ++ "."
                      askFor m xs
        Just y  -> return y