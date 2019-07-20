module AuctionBook
  ( myShuffle'
  , fillAuctionBook
  , bestToDeck
  , dropWorst
  ) where

import Data.List
import Lib

fillAuctionBook :: [a] -> [a] -> ([a], [a])
fillAuctionBook deck book = pick toPick deck book
  where
    toPick = 8 - (length book)

type Deck = [Int]

type Book = [Int]

bestToDeck :: Book -> Deck -> (Deck, Book)
bestToDeck [] deck = (deck, [])
bestToDeck book deck = (deck ++ [best], newbook)
  where
    best:newbook = reverse $ sort book

dropWorst :: Bool -> Book -> Book
dropWorst False book = book
dropWorst True [] = []
dropWorst True book = drop 1 $ sort book
