-----------------------------------------------------------------------------
--
-- Module      :  TestCLIArguments
-- Description :
-- Copyright   :  (c) Tobias Reinhardt, 2015 <tobioso92_@hotmail.com
-- License     :  Apache License, Version 2.0
--
-- Maintainer  :  Tobias Reinhardt <tobioso92_@hotmail.com>
-- Portability :  tested only on linux
-- |
--
-----------------------------------------------------------------------------

module TestCLIArguments (
    describtion
) where

import           CLIArguments
import           Control.DeepSeq   (force)
import           Control.Exception (evaluate)
import           Test.Hspec (it, shouldBe, shouldThrow, anyErrorCall)


options :: [(Option, NeedForArgument)]
options = [(Short 'a', None),
           (Long "delete", None),
           (Both 'b' "base", None),
           (Short 'c', None),
           (Both 'x' "remove", Compulsory),
           (Short 'y', Compulsory),
           (Short 'z', Compulsory),
           (Long "use", Optional "default1"),
           (Both 'v' "vee", Optional "default2"),
           (Short 'w', Optional "default3"),
           (Long "document", Compulsory),
           (Long "reset", None)]


describtion = do
  it "Parses the arguments give to a program. For example, [program] -wLr --rows 3" $
    True `shouldBe` True

  it "Follows the POSIX program Argument syntax conventions" $
    True `shouldBe` True

  it "Refer to http://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html for more details" $
    True `shouldBe` True

  it "..." $
    True `shouldBe` True

  it "Parses options and their arguments" $ do
    let args = ["-x", "arg1", "--use=arg2", "-y", "arg3"]
        marshalled = marshal args options
    fetchArgumentsOfOption (Short 'x') marshalled `shouldBe` ["arg1"]
    fetchArgumentsOfOption (Long "use") marshalled `shouldBe` ["arg2"]
    fetchArgumentsOfOption (Short 'y') marshalled `shouldBe` ["arg3"]
    fetchArgumentsOfOption (Short 'z') marshalled `shouldBe` []

  it "Parses flag options (options that takes no arguments)" $ do
    let args = ["-a", "nonOpArg1", "-c"]
        marshalled = marshal args options
        nonOptionArguments = fetchNonOptionArguments marshalled
    isOptionFlagged (Short 'a') marshalled `shouldBe` True
    isOptionFlagged (Short 'b') marshalled `shouldBe` False
    isOptionFlagged (Short 'c') marshalled `shouldBe` True
    isOptionFlagged (Short 'd') marshalled `shouldBe` False
    head nonOptionArguments `shouldBe` "nonOpArg1"

  it "Parses all non-option arguments (characters that does not follow a hyphen and are not an argument for an option)" $ do
    let args = ["-v", "arg1", "nonOpArg1", "-x", "arg2", "nonOpArg2"]
        marshalled = marshal args options
        nonOptionArguments = fetchNonOptionArguments marshalled
    length nonOptionArguments `shouldBe` 2
    head nonOptionArguments `shouldBe` "nonOpArg1"
    nonOptionArguments !! 1 `shouldBe` "nonOpArg2"

  it "Parses multiple flag options following hyphen if they don't need an argument" $ do
    let args = ["-y", "arg1", "-avc", "-z", "arg2"]
        marshalled = marshal args options
    isOptionFlagged (Short 'a') marshalled `shouldBe` True
    isOptionFlagged (Short 'c') marshalled `shouldBe` True
    isOptionFlagged (Short 'v') marshalled `shouldBe` False
    fetchArgumentsOfOption (Short 'v') marshalled `shouldBe` ["default2"]

  it "Assigns an default argument to an option if no argument is given and the argument is optional" $ do
    let args = ["--use", "-v"]
        marshalled = marshal args options
    fetchArgumentsOfOption (Long "use") marshalled `shouldBe` ["default1"]
    fetchArgumentsOfOption (Short 'v') marshalled `shouldBe` ["default2"]

  it "Throws an error if an option needing an argument is not given one" $ do
    let args1 = ["-x"]
        args2 = ["-x", "-a"]
    (evaluate . force) (show $ marshal args1 options) `shouldThrow` anyErrorCall
    (evaluate . force) (show $ marshal args2 options) `shouldThrow` anyErrorCall

  it "Throws an error if option is not defined" $ do
    let args1 = ["-o", "arg1"]
        args2 = ["--ma"]
    (evaluate . force) (show $ marshal args1 options) `shouldThrow` anyErrorCall
    (evaluate . force) (show $ marshal args2 options) `shouldThrow` anyErrorCall

  it "Throws an error if a long option that does not take any argument is however given one by an equal sign" $ do
    let args = ["--delete=arg1", "-a"]
    (evaluate . force) (show $ marshal args options) `shouldThrow` anyErrorCall

  it "Throws an error if an long option is given too many arguments by equal signs" $ do
    let args = ["--use=arg1=arg2", "-a"]
    (evaluate . force) (show $ marshal args options) `shouldThrow` anyErrorCall
