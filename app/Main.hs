module Main where

import Control.Monad (forM_)

import Solution

cases :: [(String, Keypad)]
cases = [ ("", "123456789")
        , ("4", "123456789")
        , ("4", "918273645")
        , ("45262", "192837465")
        , ("777777777777", "987654321")
        , ("2873468943752938", "789456123")
        , ("324587Q9539333", "123789456")
        , ("R", "132978645")
        , ("ZT", "192837465")
        ]

runCase :: (String, Keypad) -> Either String Int
runCase = uncurry totalDistance

main :: IO ()
main = do
  forM_ cases $ putStrLn . show . runCase
