-----------------------------------------------------------------------------
--
-- Module      :  Query
-- Description :
-- Copyright   :  (c) Tobias Reinhardt, 2015 <tobioso92_@hotmail.com
-- License     :  Apache License, Version 2.0
--
-- Maintainer  :  Tobias Reinhardt <tobioso92_@hotmail.com>
-- Portability :  tested only on linux
-- |
--
-----------------------------------------------------------------------------

module Query (
    fetchNonOptionArguments,
    isOptionFlagged,
    fetchArgumentsOfOption
) where

import Marshaller

fetchArgumentsOfOption :: Option -> [MarshalledEntity] -> [String]
fetchArgumentsOfOption _ [] = []
fetchArgumentsOfOption x (OptionWithArgument y z:zs)
  | x == y    = z : fetchArgumentsOfOption x zs
  | otherwise = fetchArgumentsOfOption x zs
fetchArgumentsOfOption x (_:ys) = fetchArgumentsOfOption x ys



fetchNonOptionArguments :: [MarshalledEntity] -> [String]
fetchNonOptionArguments [] = []
fetchNonOptionArguments (NonOptionArgument x:xs) = x : fetchNonOptionArguments xs
fetchNonOptionArguments (_:xs) = fetchNonOptionArguments xs


isOptionFlagged :: Option -> [MarshalledEntity] -> Bool
isOptionFlagged _ []                = False
isOptionFlagged x (OnlyOption y:ys) = x == y || isOptionFlagged x ys
isOptionFlagged x (_:ys)            = isOptionFlagged x ys



-- variants should be as data
