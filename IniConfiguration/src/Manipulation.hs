-----------------------------------------------------------------------------
--
-- Module      :  Manipulation
-- Description :
-- Copyright   :  (c) Tobias Reinhardt, 2015 <tobioso92_@hotmail.com
-- License     :  Apache License, Version 2.0
--
-- Maintainer  :  Tobias Reinhardt <tobioso92_@hotmail.com>
-- Portability :  tested only on linux
-- |
--
-----------------------------------------------------------------------------

module Manipulation(
	getDefaultSection,
	lookupIndex,
	removeProperty
)where

import           Data.Maybe (fromMaybe)
import           Types

getDefaultSection :: [Section] -> [Property]
getDefaultSection xs = fromMaybe [] (lookup "" xs)


lookupIndex :: Eq a => a -> [(a,b)] -> Maybe Int
lookupIndex = lookupIndex' 0

lookupIndex' :: Eq a => Int -> a -> [(a,b)] -> Maybe Int
lookupIndex' _ _ [] = Nothing
lookupIndex' i x ((y1,_):ys)
  | x == y1   = Just i
	| otherwise = lookupIndex' (i+1) x ys


removeProperty :: Eq a => a -> [(a, b)] -> [(a, b)]
removeProperty _ [] = []
removeProperty x (y@(y1,_):ys)
	| x == y1   = ys
	| otherwise = y : removeProperty x ys
