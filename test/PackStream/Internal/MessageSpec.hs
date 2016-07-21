{-# LANGUAGE OverloadedStrings #-}
module PackStream.Internal.MessageSpec where

import Control.Monad (guard, unless)
import Data.Bits
import qualified Data.ByteString as B
import Data.Char (chr)
import Data.Either.Combinators (fromRight')
import Data.Int
import Data.List (foldl')
import Data.Maybe (fromJust)
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import Data.Word

import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC hiding ((.&.))

import PackStream.Internal.Message

--------------------------------------------------------------------------------
-- Initialization bytes
nullInit :: B.ByteString
nullInit = B.singleton 0xC0 

float64Init :: B.ByteString
float64Init = B.singleton 0xC1

int8Init :: B.ByteString
int8Init = B.singleton 0xC8

int16Init :: B.ByteString
int16Init = B.singleton 0xC9

int32Init :: B.ByteString
int32Init = B.singleton 0xCA

int64Init :: B.ByteString
int64Init = B.singleton 0xCB


spec :: Spec
spec = do
  hasMarkerSpec *> intSpec *> nullSpec *> floatSpec *> textSpec

hasMarkerSpec = do 
  describe "hasMarker" $ do
    let val = "1" :: B.ByteString
    let byte = 0x77
    let marker = MarkerByte byte
    let incorrectMarker = MarkerByte 0x11
    let bs = B.cons byte val

    it "hasMarker does not fail when given a correct marker" $ do
      let decoder = S.label "Test marker byte" $ hasMarker marker *> S.getWord8 
      S.runGet decoder bs `shouldBe` Right (B.head val)
      
    it "hasMarker fails when given an incorrect marker" $ do
      let decoder = S.label "Test marker byte" $ hasMarker incorrectMarker *> S.getWord8 
      S.runGet decoder bs `shouldBe` Left "Failed reading: Incorrect marker.\nFrom:\tTest marker byte\n\n" 

nullSpec =  
  describe "Null type" $ do
    it "packNull gives the correct marker byte" $ do
      S.runPut packNull `shouldBe` nullInit 
    
    it "unpackNull gives ()" $ do
      S.runGet unpackNull (B.append nullInit "test") `shouldBe` Right ()

floatSpec = do 
  describe "Float64 Type" $ do
    it "packFloat64 gives the correct marker byte and value" $ do
      S.runPut (packFloat64 2.0) `shouldBe` (B.append float64Init (S.runPut (S.putFloat64be 2.0)))
  
    it "unpackFloat64be gives the correct values when used to decode" $ do
      S.runGet unpackFloat64 (B.append float64Init (S.runPut (S.putFloat64be 2.0))) `shouldBe` Right 2.0

data IntTypeTest a b = IntTypeTest
  { intPacklbl :: String
  , intUnpacklbl :: String
  , intVal :: a
  , intInitByte :: B.ByteString
  , intPutter :: S.Putter b
  , intPacker :: a -> S.Put
  , intUnpacker :: S.Get a
  } 

int8TestData = IntTypeTest 
  "packInt8" "unpackInt8" (2 :: Int8) 
    int8Init S.putWord8 packInt8 unpackInt8 

int16TestData = IntTypeTest 
  "packInt16" "unpackInt16" (6000 :: Int16) 
    int16Init S.putWord16be packInt16 unpackInt16

int32TestData = IntTypeTest
  "packInt32" "unpackInt32" (967296 :: Int32) 
    int32Init S.putWord32be packInt32 unpackInt32 

int64TestData = IntTypeTest
  "packInt64" "unpackInt64" (2147483650 :: Int64) 
    int64Init S.putWord64be packInt64 unpackInt64

intSpec = do 
  describe "Int8 Type" (intTypeTest int8TestData) 
  describe "Int16 Type" (intTypeTest int16TestData) 
  describe "Int32 Type" (intTypeTest int32TestData) 
  describe "Int64 Type" (intTypeTest int64TestData) 


intTypeTest (IntTypeTest packlbl unpacklbl val initByte putter packer unpacker) = do 
  let intBs = B.append initByte (S.runPut (putter (fromIntegral val)))
  it packlbl $ do
    S.runPut (packer val) `shouldBe` intBs
  
  it unpacklbl $ do
    S.runGet unpacker intBs `shouldBe` Right val

--------------------------------------------------------------------------------
-- Text tests.
tinyTextMarker = 0x80
text8Marker = 0xD0
text16Marker = 0xD1
text32Marker = 0xD2

initTinyText :: B.ByteString
initTinyText = B.singleton tinyTextMarker

data TextDataExpect = TextDataExpect
  { tdeMarker :: Word8
  , tdeSize   :: Maybe Int
  , tdeBytes  :: Maybe B.ByteString
  } deriving (Eq, Show)

data TextData = TextData
  { tdVal :: T.Text
  , tdExpect :: TextDataExpect
  } deriving (Eq, Show)

instance Arbitrary TextData where
  arbitrary = do  
      txt <- genText
      pure $ TextData txt (expectedText txt)

genText :: Gen T.Text
genText = T.pack <$> listOf1 (chr <$> choose (1,126))


-- Takes Text data and returns what shoud be expected when packing that Text
-- data.
expectedText :: T.Text -> TextDataExpect 
expectedText txt = 
  if T.null txt then 
    TextDataExpect tinyTextMarker Nothing Nothing
  else let bytes = T.encodeUtf8 txt
  in  case B.length bytes of
    size | size <= 15 -> 
      TextDataExpect tinyTextMarker (Just size) (Just bytes)
    size | size <= 255 -> 
      TextDataExpect text8Marker (Just size) (Just bytes)
    size | size <= 65535 -> 
      TextDataExpect text16Marker (Just size) (Just bytes)
    size | size <= 4294967295 -> 
      TextDataExpect text32Marker (Just size) (Just bytes)


-- Extract the higher order nibble from the given byte.
-- If the byte is 0x89 (10001001) then extract the four bits 1000.
highNibble :: Word8 -> Word8
highNibble byte = byte `shiftR` 4


-- Extract the lower order nibble from the given byte.
-- If the byte is 0x89 (10001001) then extract the four bits 1001.
lowNibble :: Word8 -> Word8
lowNibble byte = byte .&. 0x0F


-- Extract a marker byte from a byte encoded with a marker and a size.
extractMarker :: Word8 -> Word8
extractMarker byte = (highNibble byte) `shiftL` 4 


-- Encoding a Text should give the correct marker byte corresponding to the
-- encoded Text's size.
prop_packingTextGivesCorrectMarkerByte :: TextData -> Bool
prop_packingTextGivesCorrectMarkerByte (TextData txt (TextDataExpect expectedMarker expectedSize _))  
  | T.null txt = initByte == expectedMarker
  | fromJust expectedSize <= 15 = extractMarker initByte == expectedMarker  
  | otherwise = initByte == expectedMarker
 where
  generatedBytes :: B.ByteString
  generatedBytes = S.runPut (packText txt)
  
  (initByte, remainder) = fromJust (B.uncons generatedBytes)


-- Encoding Text data should give the correct size value.
prop_packingTextGivesCorrectSize :: TextData -> Bool
prop_packingTextGivesCorrectSize (TextData txt (TextDataExpect _ maybeExpectedSize _))  
  | T.null txt = Nothing == maybeExpectedSize -- Always True, there is no size.
  | expectedSize <= 15 = lowNibble initByte == expectedSize
  | otherwise = size == expectedSize
 where
  generatedBytes :: B.ByteString
  generatedBytes = S.runPut (packText txt)
  
  expectedSize          = fromIntegral (fromJust maybeExpectedSize)
  (initByte, remainder) = fromJust (B.uncons generatedBytes)
  (size, _)             = fromJust (B.uncons remainder)


prop_packAndUnpackAreIsomorphic (TextData txt _) = 
  S.runGet unpackText (S.runPut (packText txt)) == Right txt

-- Test specification for Text.
textSpec = do
  describe "packText" $ do
    it "packing text gives the correct marker corresponding to its size byte" 
      (property prop_packingTextGivesCorrectMarkerByte)
    it "packing text gives the correct size" (property prop_packingTextGivesCorrectSize)
    it "packText and unpackText are isomorphic" (property prop_packAndUnpackAreIsomorphic) 

