{-# LANGUAGE OverloadedStrings #-}
module PackStream.Internal.MessageSpec where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Either.Combinators (fromRight')
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Word

import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import PackStream.Internal.Message


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Initialization bytes
int8init = 0xC8
int16init = 0xC9
int32init = 0xCA
int64init = 0xCB
tinyStringInit = 0x80
str8Init = 0xD0

spec :: Spec
spec = do 
  describe "consStringMsg" $ do
    
    it "Constructing a message from an empty string is possible" $ do
      consStringMsg "" `shouldBe` 
        (Right $ Message (Just "\128") Nothing (Just "\NUL"))

    it "Constructing a message from a string of size < 0x10 gives the correct bytestring" $ do
      consStringMsg "Test" `shouldBe` 
        (Right $ Message (Just "\132") Nothing (Just "Test"))
    
    it "Constructing a message from a string of size > 0x10 d < 0x100 " $ do  
      consStringMsg "Another test" `shouldBe` 
        (Right $ Message (Just "\140") Nothing (Just "Another test"))
    
    it ("Constructing a msg from a string of 0x10 < size < 0x100 gives a str8 message") $ do
      let msg = fromRight' (consStringMsg "Another test here.") 
      (unpackFromJust $ messageMarker msg) `shouldBe` [0xD0]
      (unpackFromJust $ messageSize msg) `shouldBe` [0x12]
      fromJust (message msg) `shouldBe` "Another test here."

  describe "consIntMsg, construcing an int (-16 <= i <= 127)" $ do
    it "should give a tinyint" $ do 
      let msg = consIntMsg 12
      messageMarker msg `shouldBe` Nothing 
      messageSize msg `shouldBe` Nothing 
      unpackFromJust (message msg) `shouldBe` [12]

    it "Construcing an int (-128 <= i <= -17) message gives an int8 " $ do
      let msg = consIntMsg (-100)
      unpackFromJust (messageMarker msg) `shouldBe` [int8init] 
      messageSize msg `shouldBe` Nothing 
      unpackFromJust (message msg) `shouldBe` [(-100)]
    
    it "Construcing an int (-128 <= i <= -17) message gives an int16" $ do
      let msg = consIntMsg (-32760)
      unpackFromJust (messageMarker msg) `shouldBe` [int16init] 
      messageSize msg `shouldBe` Nothing 
      unpackFromJust (message msg) `shouldBe` [(-32760)]

    it "Construcing an int (-128 <= i <= -17) message gives an int16" $ do
      let msg = consIntMsg (-2147483)
      unpackFromJust (messageMarker msg) `shouldBe` [int32init] 
      messageSize msg `shouldBe` Nothing 
      unpackFromJust (message msg) `shouldBe` [(-2147483)]
    
unpackFromJust = B.unpack . fromJust


