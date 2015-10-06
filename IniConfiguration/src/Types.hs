-----------------------------------------------------------------------------
--
-- Module      :  Types
-- Description :
-- Copyright   :  (c) Tobias Reinhardt, 2015 <tobioso92_@hotmail.com
-- License     :  Apache License, Version 2.0
--
-- Maintainer  :  Tobias Reinhardt <tobioso92_@hotmail.com>
-- Portability :  tested only on linux
-- |
--
-----------------------------------------------------------------------------

module Types(
	Key,
	Value,
	Property,
	Section
)where

type Key = String
type Value = String
type Property = (Key, Value)
type Section = (String, [Property])
