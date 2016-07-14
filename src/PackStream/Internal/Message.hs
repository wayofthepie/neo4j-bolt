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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module PackStream.Internal.Message (
  -- * Null
  packNull
  , unpackNull

  , MarkerByte(..)
  , hasMarker

  -- * Float64
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
  ) where

import Control.Monad (guard, unless)
import qualified Data.ByteString.Char8 as C
import Data.Int
import qualified Data.Map as M
import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Serialize.IEEE754
import qualified Data.Text as T
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

packNull :: Put 
packNull = put mkNullByte

unpackNull :: Get ()
unpackNull = label "Unpacking Null." $ hasMarker mkNullByte 

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

--------------------------------------------------------------------------------
