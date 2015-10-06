-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Description :
-- Copyright   :  (c) Tobias Reinhardt, 2015 <tobioso92_@hotmail.com
-- License     :  Apache License, Version 2.0
--
-- Maintainer  :  Tobias Reinhardt <tobioso92_@hotmail.com>
-- Portability :  tested only on linux
-- |
--
-----------------------------------------------------------------------------

module Main (
  main
) where


import           Test.Hspec
import qualified TestDecoder
import qualified TestEncoder
import qualified TestIniConfiguration

main :: IO ()
main = do hspec $ describe "IniConfiguration" TestIniConfiguration.describtion
          hspec $ describe "Decoder" TestDecoder.describtion
          hspec $ describe "Encoder" TestEncoder.describtion
