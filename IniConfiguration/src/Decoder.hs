-----------------------------------------------------------------------------
--
-- Module      :  Decoder
-- Description :
-- Copyright   :  (c) Tobias Reinhardt, 2015 <tobioso92_@hotmail.com
-- License     :  Apache License, Version 2.0
--
-- Maintainer  :  Tobias Reinhardt <tobioso92_@hotmail.com>
-- Portability :  tested only on linux
-- |
--
-----------------------------------------------------------------------------

module Decoder(
  decode,
  readConfiguration
)where

import           Types

type Name = String
type CurrentSection = Section
type UnParsed = String

readConfiguration :: FilePath -> IO [Section]
readConfiguration x = do content <- readFile x
                         return (decode content)

decode :: String -> [Section]
decode xs = decode' (dropWhitespace xs) ("", [])

decode' :: String -> CurrentSection -> [Section]
decode' [] x          = x:[]
decode' ('[':xs) y    = let (name, unparsed) = decodeParseName "" xs
                        in y : decode' unparsed (name, [])
decode' xs (name, ys) = let (property, unparsed) = decodeProperty xs
                        in decode' unparsed (name, ys ++ [property])

decodeProperty :: String -> (Property, String)
decodeProperty xs = let (key, unparsed1)   = decodeKey "" xs
                        (value, unparsed2) = decodeValue "" unparsed1
                    in ((key, value), unparsed2)


decodeParseName :: Name -> String -> (Name, UnParsed)
decodeParseName [] (']':_) = error "There is no section name"
decodeParseName _ []       = error "Missing ']' from section name"
decodeParseName x (']':ys) = (checkForWhitespace "Section" x, dropWhitespace ys)
decodeParseName x (y:ys)   = decodeParseName (x ++ [y]) ys


decodeKey :: Name -> String -> (Key, UnParsed)
decodeKey x ('=':ys)  = (checkForWhitespace "Key " x, ys)
decodeKey _ ('\n':_)  = error ""
decodeKey x (y:ys)    = decodeKey (x ++ [y]) ys
decodeKey x _         = error ("Value does not exist for key '" ++ x ++ "'")

decodeValue :: Name -> String -> (Value, UnParsed)
decodeValue x ('\n':ys) = (checkForWhitespace "Value" $ dropWhitespaceFromEnd x, dropWhitespace ys)
decodeValue x []        = (checkForWhitespace "Value" $ dropWhitespaceFromEnd x, [])
decodeValue _ ('=':_)   = error "There are more than one equal sign on a row"
decodeValue x (y:ys)    = decodeValue (x ++ [y]) ys

checkForWhitespace :: String -> String -> String
checkForWhitespace x y = check (not . containsWhitespace) (x ++ " contains whitespace") y

whitespace :: String
whitespace = "\n \r\t"


isWhitespace :: Char -> Bool
isWhitespace x = x `elem` whitespace

check :: (a -> Bool) -> String -> a -> a
check f xs y = if f y then y else error xs


dropWhitespace :: String -> String
dropWhitespace = dropWhile isWhitespace

dropWhitespaceFromEnd :: String -> String
dropWhitespaceFromEnd = reverse . dropWhitespace . reverse

containsWhitespace :: String -> Bool
containsWhitespace [] = False
containsWhitespace (x:xs) = isWhitespace x || containsWhitespace xs
