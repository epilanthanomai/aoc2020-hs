module Main where

import AocUtil
  ( testAndRun2
  , linesOf
  , number
  )

main :: IO ()
main = testAndRun2 inputData
                   (productOfTotal 2 2020) 514579
                   (productOfTotal 3 2020) 241861950
    where
      inputData = linesOf number
      productOfTotal count target ns =
        case findWithSum count target ns of
          Nothing -> error $ "No " ++ show count ++ " items with total " ++ show target
          Just ns -> foldr (*) 1 ns


findWithSum :: (Integral a, Eq b, Num b) => a -> b -> [b] -> Maybe [b]
findWithSum 1 target ns = if elem target ns then Just [target] else Nothing
findWithSum count target [] = Nothing
findWithSum count target (n : ns) =
  case findWithSum (count - 1) (target - n) ns of
    Nothing -> findWithSum count target ns
    Just rs -> Just $ n : rs
