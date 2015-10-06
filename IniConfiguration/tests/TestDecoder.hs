-----------------------------------------------------------------------------
--
-- Module      :  TestDecoder
-- Description :
-- Copyright   :  (c) Tobias Reinhardt, 2015 <tobioso92_@hotmail.com
-- License     :  Apache License, Version 2.0
--
-- Maintainer  :  Tobias Reinhardt <tobioso92_@hotmail.com>
-- Portability :  tested only on linux
-- |
--
-----------------------------------------------------------------------------

module TestDecoder (
    describtion
) where

import           Control.DeepSeq   (force)
import           Control.Exception (evaluate)
import           IniConfiguration
import           Test.Hspec


-- TODO Make the describtion texts better

describtion = do

  it "Has a default section where all unsectioned options are stored" $ do
    let result = decode "op1=val1\nop2=val2"
    let Just properties = lookup "" result
    lookup "op1" properties `shouldBe` Just "val1"
    lookup "op2" properties `shouldBe` Just "val2"
    lookup "op3" properties `shouldBe` Nothing


  it "Can decode text where there is a section together with an unsectioned part" $ do
    let result = decode "op1=val1\n\
	                      \op2=val2\n\
                        \ \n\
                        \[section1]\n\
                        \op3=val3\n\
                        \op4=val4\n\
                        \op5=val5"
    let Just unsectionedProperties = lookup "" result
    let Just sectionedProperties = lookup "section1" result
    lookup "op1" unsectionedProperties `shouldBe` Just "val1"
    lookup "op2" unsectionedProperties `shouldBe` Just "val2"
    lookup "op3" sectionedProperties `shouldBe` Just "val3"
    lookup "op4" sectionedProperties `shouldBe` Just "val4"
    lookup "op5" sectionedProperties `shouldBe` Just "val5"


  it "Can have multiple sections" $ do
    let result = decode "[section1]\n\
	                    \op1=val1\n\
	                    \op2=val2\n\
	                    \ \n\
	                    \[section2]\n\
	                    \op3=val3\n\
	                    \op4=val4\n\
	                    \ \n\
	                    \[section3]\n\
	                    \op5=val5\n"
    let Just section1 = lookup "section1" result
    let Just section2 = lookup "section2" result
    let Just section3 = lookup "section3" result
    (lookup "op1" section1) `shouldBe` Just "val1"
    (lookup "op2" section1) `shouldBe` Just "val2"
    (lookup "op3" section2) `shouldBe` Just "val3"
    (lookup "op4" section2) `shouldBe` Just "val4"
    (lookup "op5" section3) `shouldBe` Just "val5"


  it "Strips empty lines and trailing whitespaces" $ do
    let result = decode "  [section1]  \n\
	                    \   \n\
	                    \  op1=val1  \n\
	                    \ \n  \
	                    \ op2=val2 \n\
	                    \ [section2] \n\
	                    \ op3=val3 \n\
	                    \ \n"
    let Just section1 = lookup "section1" result
    let Just section2 = lookup "section2" result
    (lookup "op1" section1) `shouldBe` Just "val1"
    (lookup "op2" section1) `shouldBe` Just "val2"
    (lookup "op3" section2) `shouldBe` Just "val3"


  it "keys are allowed to have empty values" $ do
    let result = decode "op1=\n"
    let Just section = lookup "" result
    (lookup "op1" section) `shouldBe` Just ""


  it "Throws an error if equal sign is missing" $ do
    (evaluate . force) (show $ decode "op1") `shouldThrow` anyErrorCall


  it "Throws an error if section contains whitespace" $ do
    (evaluate . force) (show $ decode " [ section1]") `shouldThrow` anyErrorCall
    (evaluate . force) (show $ decode "[secti on1]") `shouldThrow` anyErrorCall
    (evaluate . force) (show $ decode "[section1 ]") `shouldThrow` anyErrorCall


  it "Throws an error if property contains whitespace" $ do
	(evaluate . force) (show $ decode "o p1=val1") `shouldThrow` anyErrorCall
	(evaluate . force) (show $ decode "op1 =val1") `shouldThrow` anyErrorCall
	(evaluate . force) (show $ decode "op1= val1") `shouldThrow` anyErrorCall
	(evaluate . force) (show $ decode "op1=va l1") `shouldThrow` anyErrorCall


  it "property contains more than one equal sign" $ do
	(evaluate . force) (show $ decode "op1=val1=val2") `shouldThrow` anyErrorCall
