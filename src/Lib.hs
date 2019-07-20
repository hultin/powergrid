module Lib
  ( someFunc
  , pick
  , randomList
  , myShuffle
  , myShuffle'
  ) where

import System.Random
import System.Random.Shuffle

someFunc :: IO ()
someFunc = putStrLn "someFunc"

pick :: Int -> [a] -> [a] -> ([a], [a])
pick n xs ys = (xsl, ys ++ xsf)
  where
    (xsf, xsl) = splitAt n xs

-- remove :: a -> [a] -> [a]
-- remove c deck = 
-- ranN :: Int
-- ranN = do
--   num <- randomIO :: IO Int
--   return num
randomList :: (Random a) => Int -> [a]
randomList seed = randoms (mkStdGen seed)

myShuffle :: [a] -> [a]
myShuffle xs = shuffle' xs (length xs) (mkStdGen 0)

myShuffle' :: [a] -> [Int] -> [a]
myShuffle' xs g = shuffle xs g

-- l <- shuffleM xs
