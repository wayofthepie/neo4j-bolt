{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Either.Combinators (fromRight')
import Data.Maybe (fromJust)
import Data.Proxy
import qualified Data.Text as T
import Data.Tuple (swap)
import Data.Word
import Network.Socket
import Network.Socket.ByteString hiding (send, sendTo, recv, recvFrom) 
import System.IO.Unsafe 

import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import qualified Neo4j as N
import PackStream.Internal.Message

main :: IO ()
main = tests >>= defaultMain  

tests :: IO TestTree
tests = do
  s <- testSpec "" spec 
  pure $ testGroup "Tests"
    [ properties
    , s
    ] 

properties :: TestTree
properties = testGroup "verifyVersion properties" 
  [ testProperty "calling with a server version of 0 should return an error" (once propVerifyServerVersionOfZero)
  --, testProperty "calling with a server version > 0 should return the socket" propVerifyServerVersion
  ]

data VersionZero = VersionZero 
  { zeroVer :: C.ByteString
  } deriving (Eq, Show)

instance Arbitrary VersionZero where
  arbitrary = consVersionZero <$> choose (0,0)

consVersionZero :: Int -> VersionZero
consVersionZero zero = VersionZero (C.pack $ show zero)

propVerifyServerVersionOfZero :: VersionZero -> Bool
propVerifyServerVersionOfZero (VersionZero v) = 
  N.verifyServerVersion v == 
    Left "No protocol version could be agreed"

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

  describe "consIntMsg" $ do
    it "Construcing an int (-16 <= i <= 127) message gives a tinytint" $ do
      let msg = consIntMsg 12
      messageMarker msg `shouldBe` Nothing 
      messageSize msg `shouldBe` Nothing 
      unpackFromJust (message msg) `shouldBe` [12]
    
unpackFromJust = B.unpack . fromJust


