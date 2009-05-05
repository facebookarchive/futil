-- swap ith and jth words on every line
-- (keeping any word-trailing whitespace with the word)
-- usage: wordSwap i j
-- note that if there is leading whitespace, the first "word" is ""

import Data.Array
import Data.Char
import System.Environment
import System.IO
import FUtil

wordsWithWS :: String -> [String]
wordsWithWS [] = []
wordsWithWS s = let
  (a, b) = span (not . isSpace) s
  (b1, b2) = span isSpace b
  in (a ++ b1):wordsWithWS b2

swapIndices :: Int -> Int -> [a] -> [a]
swapIndices i j xs = elems $
  listArray (1, length xs) xs // [(i, xs !! (j - 1)), (j, xs !! (i - 1))]

swapWords :: Int -> Int -> String -> String
swapWords i j = concat . swapIndices i j . wordsWithWS

main = do
  args <- getArgs
  case sequence $ map readMb args of
    Just [i, j] -> interactL $ map (swapWords i j)
    _ -> hPutStrLn stderr "usage"
