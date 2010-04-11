module Main where

import Data.Char
import Data.List
import Test.QuickCheck
import FUtil

chkInterBreaks :: Char -> [Char] -> Bool
chkInterBreaks c s = intercalate [c] (breaks (== c) s) == s

chkCalate :: [Char] -> [Char] -> Bool
chkCalate glue s = s == intercalate glue (uncalate glue s)

main = do
  quickCheck chkInterBreaks
  quickCheck chkCalate
