-----------------------------------------------------------------------------
--
-- Module      :  TestIniConfiguration
-- Description :
-- Copyright   :  (c) Tobias Reinhardt, 2015 <tobioso92_@hotmail.com
-- License     :  Apache License, Version 2.0
--
-- Maintainer  :  Tobias Reinhardt <tobioso92_@hotmail.com>
-- Portability :  tested only on linux
-- |
--
-----------------------------------------------------------------------------

module TestIniConfiguration (
    describtion
) where

import           Control.DeepSeq   (force)
import           Control.Exception (evaluate)
import           Test.Hspec


describtion = do

-- TODO Add tests for reading and wirting a file

  it "Can read INI configuration files" $ do
    True `shouldBe` True

  it "Can write INI configuration files" $ do
    True `shouldBe` True

  it "Can also directly decode and encode strings in INI format" $ do
    True `shouldBe` True

  it "Refer to https://en.wikipedia.org/wiki/INI_file for more information about INI format" $ do
    True `shouldBe` True
