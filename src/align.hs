-- align code to stdin based on args
-- usage: align <string-to-align-at> [on-nth-incidence-of-string]

import System.Environment
import FUtil

-- perform a "break-like" function at the nth place instead of the first place
breaklikeNth :: Int -> [a] -> ([a] -> Maybe ([a], [a])) -> [a] ->
  Maybe ([a], [a])
breaklikeNth 1 _ f xs = f xs
breaklikeNth n glue f xs = do
  (l, r) <- breaklikeNth (n - 1) glue f xs
  (rl, rr) <- f r
  return (l ++ glue ++ rl, rr)

-- take a non-hacky break-like function: returning  Maybe (l, r)
-- and make it hacky like break is: returning  (l, glue ++ r)
breaklikeHack :: [a] -> ([a] -> Maybe ([a], [a])) -> [a] -> ([a], [a])
breaklikeHack glue f xs = case f xs of
  Nothing     -> (xs, [])
  Just (l, r) -> (l, glue ++ r)

breakOnNthSublHacky :: Eq a => Int -> [a] -> [a] -> ([a], [a])
breakOnNthSublHacky n s = breaklikeHack s (breaklikeNth n s (breakOnSubl s))

alignOn :: Int -> String -> [String] -> [String]
alignOn n s ls = map unbreak (zip (spaceBlock p1) p2) where
  (p1, p2) = unzip (map (breakOnNthSublHacky n s) ls)
  unbreak (a, b) = a ++ b

main = do
  as <- getArgs
  let
    argN = length as
    n = if argN == 1 then Just 1 else if argN == 2
      then Just $ read $ as !! 1
      else Nothing
  case n of
    Nothing -> putStrLn "usage"
    Just n -> interactL $ alignOn n (head as)
