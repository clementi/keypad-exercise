module Main where

import Solution

main = do
  let pad = "987123645"
  let validStr = "7635894353168834659896"
  let invalidStr = "435786345Q34589347"
  let validDist = totalDistance validStr pad
  let invalidDist = totalDistance invalidStr pad
  putStrLn $ "totalDistance('" ++ validStr ++ "', '" ++ pad ++ "') = " ++ show validDist
  putStrLn $ "totalDistance('" ++ invalidStr ++ "', '" ++ pad ++ "') = " ++ show invalidDist
