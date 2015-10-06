-----------------------------------------------------------------------------
--
-- Module      :  TestEncoder
-- Description :
-- Copyright   :  (c) Tobias Reinhardt, 2015 <tobioso92_@hotmail.com
-- License     :  Apache License, Version 2.0
--
-- Maintainer  :  Tobias Reinhardt <tobioso92_@hotmail.com>
-- Portability :  tested only on linux
-- |
--
-----------------------------------------------------------------------------

module TestEncoder(
	describtion
) where

--import		   Control.DeepSeq   (force)
--import		   Control.Exception (evaluate)
import		   IniConfiguration
import		   Test.Hspec


describtion = do
  it "Properties of default section are the first elements to be decoded" $ do
	let result = encode [("section1", [("op1", "val1"), ("op2", "val2")]),
			   			 ("", [("op3", "val3"), ("op4", "val4")]),
					     ("section2", [("op5", "val5")])]
	result `shouldBe` "op3=val3\nop4=val4\n\n[section1]\n\nop1=val1\nop2=val2\n\n[section2]\n\nop5=val5\n"
