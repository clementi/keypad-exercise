module Solution (Keypad, totalDistance) where

import Control.Applicative (liftA2)
import Control.Monad (join, sequence)
import Data.Bifunctor (bimap)
import Data.Either.Utils (maybeToEither)
import Data.List (elemIndex)

type Keypad = String
type Location = (Int, Int)

totalDistance :: String -> Keypad -> Either String Int
totalDistance s keypad = sum <$> sequence dists
  where dists = map (uncurry (liftA2 distance)) steps
        steps = zip locs $ tail locs
        locs = map (location keypad) s

distance :: Location -> Location -> Int
distance left right = uncurry max $ pairAbs $ diff left right
  where diff a b = (fst a - fst b, snd a - snd b)
        pairAbs = join bimap abs

location :: Keypad -> Char -> Either String Location
location keypad c = coords <$> maybeToEither ("Invalid character " ++ show c) (elemIndex c keypad)
  where coords index = (index `div` size, index `mod` size)
        size = truncate $ sqrt $ fromIntegral $ length keypad
