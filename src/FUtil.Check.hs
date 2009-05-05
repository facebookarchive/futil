module Main where

import Data.Char
import Data.List
import Test.QuickCheck
import FUtil

instance Arbitrary Char where
    arbitrary     = choose ('a', 'b')
    coarbitrary c = variant (ord c `rem` 4)

chkInterBreaks :: Char -> [Char] -> Bool
chkInterBreaks c s = intercalate [c] (breaks (== c) s) == s

chkCalate :: [Char] -> [Char] -> Bool
chkCalate glue s = s == intercalate glue (uncalate glue s)

{-
chks = [chkInterBreaks, chkDblTransp]
-}

--chkF = quickCheck
--chkF = verboseCheck

main = do
  -- todo: TH to at least get chkF..
  quickCheck chkInterBreaks
  verboseCheck chkCalate
