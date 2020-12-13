module Main where

import AocUtil
  ( testAndRun
  , linesOf
  , number
  )

main :: IO ()
main = testAndRun inputData (pairProduct . findPairWithSum 2020) 514579
    where
      inputData = linesOf number
      pairProduct Nothing = error "No pair totaling 2020"
      pairProduct (Just (a, b)) = a * b



findPairWithSum :: (Eq a, Num a) => a -> [a] -> Maybe (a, a)
findPairWithSum target [] = Nothing
findPairWithSum target (n : ns) =
  if elem (target - n) ns
     then Just (n, target - n)
     else findPairWithSum target ns
