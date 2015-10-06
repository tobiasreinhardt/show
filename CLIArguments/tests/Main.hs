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


import qualified TestCLIArguments
import Test.Hspec

main :: IO ()
main = hspec $ describe "CLIArguments" TestCLIArguments.describtion
