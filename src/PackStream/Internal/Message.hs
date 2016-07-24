{-|
  Module      : PackStream
  Description : Implementation of Neo4j's PackStream for the bolt protocol.
  Copyright   : (c) 2016, Stephen O'Brien
  License     : BSD3
  Maintainer  : Stephen O'Brien <wayofthepie@gmail.com>
  Stability   : experimental
  Portability : non-portable
  Basic implementation of packstream message data types.
  @__Warning__@: This is a work in progress and is currently __very__ experimental.

  Packstream is a message serialization formatused in neo4j's bolt protocol.

-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module PackStream.Internal.Message (
  -- * Null
  -- $null
  packNull
  , unpackNull

  , MarkerByte(..)
  , hasMarker

  -- * Float64
  -- $float
  , packFloat64
  , unpackFloat64

  -- * Integer
  -- $integer
  , packInt8
  , packInt16
  , packInt32
  , packInt64
  , unpackInt8
  , unpackInt16
  , unpackInt32
  , unpackInt64

  -- * Text
  -- $text
  , packText
  , unpackText

  -- * Boolean
  -- $boolean
  , packBoolean
  , unpackBoolean
  ) where

import Control.Monad (guard, unless)
import Data.Bits
import qualified Data.ByteString as B
import Data.Int
import qualified Data.Map as M
import Data.Maybe (maybe)
import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Serialize.IEEE754
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (decodeUtf8, encodeUtf8)
import Data.Word
import Debug.Trace

import Prelude hiding (head, tail)

--------------------------------------------------------------------------------
-- Refinement for packstream __Int64__ constraints.
{-@ type ConstrainedInteger = { i:Integer | i >= (-9223372036854775808) && i <= 9223372036854775808 } @-}

-- | PackStream types.
data PSType =
  PSNull
  | PSBool    Bool
  | PSInt8    Int8
  | PSInt16   Int16
  | PSInt32   Int32
  | PSInt64   Int64
  | PSFloat   Double
  | PSText    T.Text
  | PSList    [PSType]
  | PSMap     (M.Map T.Text PSType)
  | PSStruct  Signature [PSType]

-- | A byte which represents the type of a PackStream Structure.
newtype Signature = Signature { signature :: Word8 } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Marker bytes

-- | A marker byte contains information on the data type as well as direct
-- or indirect size information.
newtype MarkerByte = MarkerByte { marker :: Word8 } deriving (Eq, Show)

-- | A marker byte is just a byte, which serializes to a @__'Word8'__@.
instance Serialize MarkerByte where
  put = putWord8 . marker
  get = getWord8 >>= pure . MarkerByte

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Pack and Unpack

mkNullByte = MarkerByte 0xC0
mkFloat64Byte = MarkerByte 0xC1
mkInt8Byte = MarkerByte 0xC8
mkInt16Byte = MarkerByte 0xC9
mkInt32Byte = MarkerByte 0xCA
mkInt64Byte = MarkerByte 0xCB


-- | Checks whether the marker byte @__'marker'__@ exists, if it does not we
-- fail parsing with the given message.
hasMarker :: (Eq a, Serialize a) => a -> Get ()
hasMarker marker = get >>= \got -> unless (marker == got) (fail "Incorrect marker.")

{- $null
-}
packNull :: Put
packNull = put mkNullByte

unpackNull :: Get ()
unpackNull = label "Unpacking Null." $ hasMarker mkNullByte


{- $float
-}
packFloat64 :: Double -> Put
packFloat64 d = put mkFloat64Byte *> putFloat64be d

unpackFloat64 :: Get Double
unpackFloat64 = label "Unpacking Float64" $ hasMarker mkFloat64Byte *> getFloat64be

{- $integer
  Packstream deals with integers of sizes 8, 16, 32 and 64 bytes. Each type is
  represented differently in the protocol. Integer values occupy either 1, 2, 3,
  5 or 9 bytes.
  The types of integers are TinyInt, Int8, Int16, Int32 and Int64.
-}
packIntX :: (Integral a, Num b) => a -> MarkerByte -> Putter b -> Put
packIntX i mk putter = put mk *> putter (fromIntegral i)

unpackIntX :: (Integral a, Integral b) => String -> MarkerByte -> Get a -> Get b
unpackIntX lbl mk getter = label lbl $ hasMarker mk *> (fmap fromIntegral getter)

-- | Tiny Int's are @__'Int8'__@'s in the range: /(-2^4) <= i <= (2^7)/.
--packTinyInt :: Int8

packInt8 :: Int8 -> Put
packInt8 i = packIntX i mkInt8Byte putWord8

unpackInt8 :: Get Int8
unpackInt8 = unpackIntX "Unpacking Int8" mkInt8Byte getWord8

packInt16 :: Int16 -> Put
packInt16 i = packIntX i mkInt16Byte putWord16be

unpackInt16 :: Get Int16
unpackInt16 = unpackIntX "Unpacking Int16" mkInt16Byte getWord16be

packInt32 :: Int32 -> Put
packInt32 i = packIntX i mkInt32Byte putWord32be

unpackInt32 :: Get Int32
unpackInt32 = unpackIntX "Unpacking Int32" mkInt32Byte getWord32be

packInt64 :: Int64 -> Put
packInt64 i = packIntX i mkInt64Byte putWord64be

unpackInt64 :: Get Int64
unpackInt64 = unpackIntX "Unpacking Int64" mkInt64Byte getWord64be

{- $text
  Textual data is represented as __UTF-8__ encoded binary data. There are four
  encodings possible for text, determined by the size of the data:

  [@< 2^4@] Max size of /__15__/ bytes.
    Size contained within low-order nibble of marker.

  [@< 2^8@] Max size of /__255__/ bytes.
    Size is an 8-bit big-endian unsigned integer.

  [@< 2^16@] Max size of /__65535__/ bytes.
    Size is a 16-bit big-endian unsigned integer.

  [@< 2^32@] Max size of /__4 294 967 295__/ bytes.
    Size is a 32-bit big-endian unsigned integer.

  Text containing /__< 16__/ bytes  (including empty strings), should have a marker
  byte containing the higher-order nibble /1000/ followed by a lower order nibble
  containing the size.

-}

initTinyText = 0x80
mkTinyText = MarkerByte initTinyText
mkText8 = MarkerByte 0xD0
mkText16 = MarkerByte 0xD1
mkText32 = MarkerByte 0xD2

-- | Packs @__'T.Text'__@ values of sizes /__0__/ to /__2^32__/ bytes. Sizes
-- larger than /__2^32__/ bytes will return @__'Nothing'__@.
--
-- FIXME: Size bounds can likely be checked at compile time with liquid haskell.
packText :: T.Text -> Put
packText text
  | T.null text = put mkTinyText
  | otherwise = case B.length textBytes of
      size | size <= 15 -> buildTinyText size
      size | size <= 255 -> putTextX mkText8 putWord8 size
      size | size <= 65535 -> putTextX mkText16 putWord16be size
      size | size <= 4294967295 ->  putTextX mkText32 putWord32be size
      _ -> error "Size too large!" -- FIXME : Liquid haskell check or either!!
 where
  textBytes :: B.ByteString
  textBytes = T.encodeUtf8 text

  buildTinyText :: Int -> Put
  buildTinyText size = putWord8 (tinyTextInitByte size) *> putByteString textBytes

  tinyTextInitByte :: Int -> Word8
  tinyTextInitByte size = initTinyText .|. (fromIntegral size)

  putTextX :: Num a => MarkerByte -> Putter a -> Int -> Put
  putTextX mb putter size =
    put mb *> putter (fromIntegral size) *> putByteString textBytes


-- | Unpacks @__'T.Text'__@ values.
unpackText :: Get T.Text
unpackText = label "Unpacking Text" $ do
  maybeSize <- hasTextMarker
  maybe (T.decodeUtf8 <$> (size >>= getByteString))
        ((fmap T.decodeUtf8) . getByteString . fromIntegral)
        maybeSize
 where
  size :: Get Int
  size = fromIntegral <$> getWord8

hasTextMarker :: Get (Maybe Word8)
hasTextMarker = get >>= \got -> do
  let (isText, maybeSize) = isTextMarker got
  unless isText (fail "Incorrect marker.") *> pure maybeSize

-- Whether the marker byte is a Text marker.
isTextMarker :: MarkerByte -> (Bool, Maybe Word8)
isTextMarker markerByte@(MarkerByte byte)
  | byte `shiftR` 4 == 0x08 = (True, Just tinyIntSize)
  | otherwise = ((markerByte == mkText8)
    || (markerByte == mkText16)
    || (markerByte == mkText32), Nothing)
 where
  tinyIntSize = byte `shiftL` 4 `shiftR` 4


{- $boolean
  Boolean values are encoded with a single byte marker.

  [@0xC3@] Denotes @__'True'__@.
  [@0xC4@] Denotes @__'False'__@.

-}
boolTrue = MarkerByte 0xC3
boolFalse = MarkerByte 0xC4


-- | Packs a @__'Bool'__@.
packBoolean :: Bool -> Put
packBoolean b | b = put boolTrue
              | otherwise = put boolFalse


-- | Unpack a @__'Bool'__@.
unpackBoolean :: Get Bool
unpackBoolean = label "Unpacking Boolean" hasBoolMarker


-- | Retrieve the @__'Bool'__@ value if we have one, fail the parsing if not.
hasBoolMarker :: Get Bool
hasBoolMarker = get >>= \got -> do
  let (isBool, val) = ifBoolDecode got
  unless isBool (fail "Incorrect marker.") *> pure val


-- | The @__'fst'__@ of the returned tuple will be @__'True'__@
-- if the @__'MarkerByte'__@ represents a @__'Bool'__@.
ifBoolDecode :: MarkerByte -> (Bool, Bool)
ifBoolDecode (MarkerByte byte)
  | (byte == marker boolTrue) = (True,True) -- is Bool True
  | (byte == marker boolFalse) = (True,False) -- is Bool False
  | otherwise = (False, False) -- is not a Bool


