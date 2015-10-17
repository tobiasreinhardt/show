-----------------------------------------------------------------------------
--
-- Module      :  Encoder
-- Description :
-- Copyright   :  (c) Tobias Reinhardt, 2015 <tobioso92_@hotmail.com
-- License     :  Apache License, Version 2.0
--
-- Maintainer  :  Tobias Reinhardt <tobioso92_@hotmail.com>
-- Portability :  tested only on linux
-- |
--
-----------------------------------------------------------------------------

module Encoder(
  encode,
  writeConfiguration
)where

import           Data.List (intercalate)
import           Types

writeConfiguration :: FilePath -> [Section] -> IO [Section]
writeConfiguration x ys = do writeFile x $ encode ys
                             return ys

encode :: [Section] -> String
encode xs = case (defaultSection, otherSections) of
              (Nothing, vs) -> encodeSections vs
              (Just us, []) -> encodeProperties us ++ "\n"
              (Just [], vs) -> encodeSections vs
              (Just us, vs) -> encodeProperties us ++ "\n\n" ++ encodeSections vs
  where defaultSection = lookup "" xs
        otherSections = filter (\(l,_) -> l/="") xs


encodeSections :: [Section] -> String
encodeSections []     = []
encodeSections ([x]) = encodeSection x ++ "\n"
encodeSections (x:xs) = encodeSection x ++ "\n\n" ++ encodeSections xs

encodeSection :: Section -> String
encodeSection (name,[])  = "[" ++ name ++ "]"
encodeSection (name, xs) = "[" ++ name ++ "]\n\n" ++ encodeProperties xs

encodeProperties :: [Property] -> String
encodeProperties xs = intercalate "\n" $ map encodeProperty xs

encodeProperty :: Property -> String
encodeProperty (key, value) = key ++ "=" ++ value
