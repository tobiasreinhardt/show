-----------------------------------------------------------------------------
--
-- Module      :  Marshaller
-- Description :
-- Copyright   :  (c) Tobias Reinhardt, 2015 <tobioso92_@hotmail.com
-- License     :  Apache License, Version 2.0
--
-- Maintainer  :  Tobias Reinhardt <tobioso92_@hotmail.com>
-- Portability :  tested only on linux
-- |
--
-----------------------------------------------------------------------------


module Marshaller (
      Option (Short, Long, Both),
      NeedForArgument (Optional, Compulsory, None),
      MarshalledEntity (NonOptionArgument, OptionWithArgument, OnlyOption),
      marshal,
      marshalArguments
) where


import           Data.List.Split
import           System.Environment

data NeedForArgument = Optional String | Compulsory | None
                       deriving(Show)


data Option = Short Char      |
              Long String     |
              Both Char String
              deriving(Show)


instance Eq Option where
      Short c1  == Short c2   = c1 == c2
      Long s1   == Long s2    = s1 == s2
      Short c1  == Both c2 _  = c1 == c2
      Long s1   == Both _ s2  = s1 == s2
      Both c1 _ == Short c2   = c1 == c2
      Both _ s1 == Long s2    = s1 == s2
      _         == _          = False


data MarshalledEntity = NonOptionArgument String         |
                        OptionWithArgument Option String |
                        OnlyOption Option
                        deriving(Show)


type CommandLineArgument = String
type OptionDefinition = (Option, NeedForArgument)

marshalArguments :: [OptionDefinition] -> IO [MarshalledEntity]
marshalArguments x = do args <- getArgs
                        return (marshal args x)

marshal :: [CommandLineArgument] -> [OptionDefinition] -> [MarshalledEntity]
marshal [] _ = []
marshal (('-':'-':x):xs) ys = parseLong x xs ys
marshal (('-':x:[]):xs) ys = parseShort x xs ys
marshal (('-':x):xs) ys = parseCluster x xs ys
marshal (x:xs) ys = NonOptionArgument x : marshal xs ys


parseShort :: Char -> [CommandLineArgument] -> [OptionDefinition] -> [MarshalledEntity]
parseShort z [] ys = qualifyArgumentForOption (Short z) [] ys : []
parseShort z xxs@(('-':_):_) ys = qualifyArgumentForOption (Short z) [] ys : marshal xxs ys
parseShort z xxs@(x:xs) ys      = case (getNeedForArgument (Short z) ys) of
                                    None -> OnlyOption (Short z) : marshal xxs ys
                                    _    -> OptionWithArgument (Short z) x : marshal xs ys


parseLong :: String -> [CommandLineArgument] -> [OptionDefinition] -> [MarshalledEntity]
parseLong z xs ys = case splitOn "=" z of
                      (u:[])     -> qualifyArgumentForOption (Long u) [] ys : marshal xs ys
                      (u:(v:[])) -> qualifyArgumentForOption (Long u) v ys : marshal xs ys
                      (_)        -> error "Too many arguments"


parseCluster :: String -> [CommandLineArgument] -> [OptionDefinition] -> [MarshalledEntity]
parseCluster [] xs ys     = marshal xs ys
parseCluster (z:zs) xs ys = qualifyArgumentForOption (Short z) [] ys : parseCluster zs xs ys


getNeedForArgument :: Option -> [OptionDefinition] -> NeedForArgument
getNeedForArgument x xs = case lookup x xs of
                            Nothing -> error "Option not defined"
                            Just u  -> u


qualifyArgumentForOption :: Option -> String -> [OptionDefinition] -> MarshalledEntity
qualifyArgumentForOption x y xs = case (getNeedForArgument x xs, y) of
                                    (Compulsory, []) -> error "option needs argument"
                                    (None, _:_)      -> error "option can't have an argument"
                                    (Optional u, []) -> OptionWithArgument x u
                                    (_, [])          -> OnlyOption x
                                    (_, _)           -> OptionWithArgument x y
