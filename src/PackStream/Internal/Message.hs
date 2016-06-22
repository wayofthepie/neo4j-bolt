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
-}

{-# LANGUAGE DeriveFunctor #-}
module PackStream.Internal.Message (
  -- * Message
  Message(..)
  , getMessageMarker
  , getMessageSize
  , getMessage
  -- * Message Constructors
  , consNullMsg
  , consFloat64Msg
  , consIntMsg
  , consStringMsg
  ) where

import Control.Monad (foldM)
import Data.Bits ((.|.))
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as C
import Data.Serialize.Put
import Data.Serialize.IEEE754
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import Debug.Trace

import Prelude hiding (head, tail)

--------------------------------------------------------------------------------
-- Refinement for packstream __Int64__ constraints.
{-@ type ConstrainedInteger = { i:Integer | i >= (-9223372036854775808) && i <= 9223372036854775808 } @-}

{-@ predicate NonNull X = ((len X) > 0) @-}

{-@ head   :: {v:[a] | (NonNull v)} -> a @-}
head (x:_) = x
head []    = error "Can never happen."

{-@ tail :: {v:[a] | (NonNull v)} -> [a] @-}
tail (_:xs) = xs
tail []     = error "Can never happen."


-- | A @__'Message'__@ encapsulates the __marker__ byte(s), __size__ byte(s) and
-- actual __message__ bytes in a packstream type.
data Message = Message
  { messageMarker :: Maybe C.ByteString
  , messageSize   :: Maybe C.ByteString
  , message       :: Maybe C.ByteString
  } deriving (Eq, Show)

getMessageMarker = messageMarker
getMessageSize   = messageSize
getMessage       = message


--------------------------------------------------------------------------------
-- $
-- >>> consNullMsg
-- Message {messageMarker = Just "\NUL\NUL\NUL\192", messageSize = Nothing, message = Nothing}

-- | Construct a @__'Message'__@ corresponding to a packstream __Null__
-- type.
consNullMsg :: Message
consNullMsg = Message (Just $ runPut (putWord32be 0xC0)) Nothing Nothing

--------------------------------------------------------------------------------
-- $
-- >>> consFloat64Msg 1.2
-- Message {messageMarker = Just "\NUL\NUL\NUL\193", messageSize = Nothing, message = Just "?\243\&333333"}

-- | Construct a @__'Message'__@ corresponding to a packstream __Float__ type.
consFloat64Msg :: Double -> Message
consFloat64Msg msg = Message
  (Just $ runPut (putWord32be 0xC1))
  Nothing
  (Just $ runPut (putFloat64be msg))


--------------------------------------------------------------------------------
consStringMsg :: T.Text -> Either T.Text Message
consStringMsg s = consStringMsg' s (fromIntegral $ T.length s) 

consStringMsg' :: T.Text -> Word8 -> Either T.Text Message
consStringMsg' str size
  | size < 0x10  = Right (consTinyStrMsg tinyStringInit size str) -- < 16
  | size <=0xff  = Right (consStr8Msg str8Init size str) -- < 256
  | otherwise = Left (T.pack "Error")
 where
  consTinyStrMsg :: Word8 -> Word8 -> T.Text -> Message
  consTinyStrMsg initBytes size str = Message
    (Just . word8ToBs $ (initBytes .|. size))
    Nothing
    (Just . runPut $ word8sToBin (B.unpack $ (T.encodeUtf8 str)))

  consStr8Msg :: Word8 -> Word8 -> T.Text -> Message
  consStr8Msg initBytes size str = Message
    (Just . word8ToBs $ initBytes)
    (Just $ word8ToBs size)
    (Just . runPut $ putByteString (T.encodeUtf8 str))

word8ToBs :: Word8 -> C.ByteString
word8ToBs = runPut . putWord8

word8sToBin :: [Word8] -> Put
word8sToBin []  = putWord8 0x00
word8sToBin strAsW8 = word8sToBin' (tail strAsW8) (putWord8 $ head strAsW8)


word8sToBin' :: [Word8] -> Put -> Put
word8sToBin' [] acc     = acc
word8sToBin' (x:xs) acc = word8sToBin' xs (acc <* putWord8 x) 

--------------------------------------------------------------------------------
-- Integer types

-- $
-- >>> import Data.Maybe (fromJust)
-- >>> let msg = consIntMsg 0
-- >>> messageMarker msg == Nothing
-- True
-- >>> messageSize msg == Nothing
-- True
-- >>> fromJust (message msg) == runPut (putWord8 0x00)
-- True

-- | Construct a @__'Message'__@ corresponding to a packstream __TINY_INT__, which
-- is of size between -2^4 and +2^7.
--
-- Note that this currently does not support encoding in any value, i.e. an int
-- between -16 and 127 __must__ be a __TINY_INT__ event though packstream supports
-- a wider encoding.
--
--
{-@ consIntMsg :: ConstrainedInteger -> Message @-}
consIntMsg :: Integer -> Message
consIntMsg i = consActualMsg i
 where
  consActualMsg ::Integer -> Message
  consActualMsg  i
    | isTinyInt i = consTinyIntMsg i -- -16 <= i <= 127
    | isInt8 i    = consInt8Msg i  -- -128 <= i <= -17 
    | isInt16 i   = consInt16Msg i -- -32768 <= i <= 32768
    | isInt32 i   = consInt32Msg i -- -2147483648 <= i <= 2147483648
    | isInt64 i   = consInt64Msg i -- -9223372036854775808 <= i <= 9223372036854775808
    | otherwise   = undefined -- Not possible, checked with liquid haskell

consTinyIntMsg :: Integer -> Message
consTinyIntMsg i = Message
  Nothing
  Nothing
  (Just $ runPut (putWord8 (fromIntegral i)))

consInt8Msg :: Integer -> Message
consInt8Msg i = Message
  (Just $ runPut (putWord8 int8init))
  Nothing
  (Just $ runPut (putWord8 (fromIntegral i)))

consInt16Msg :: Integer -> Message
consInt16Msg i = Message
  (Just $ runPut (putWord8 int16init))
  Nothing
  (Just $ runPut (putWord16be (fromIntegral i)))

consInt32Msg :: Integer -> Message
consInt32Msg i = Message
  (Just $ runPut (putWord8 int32init))
  Nothing
  (Just $ runPut (putWord32be (fromIntegral i)))

-- >>> :t message (consInt64Msg int64Max)
-- Just Word64
consInt64Msg :: Integer -> Message
consInt64Msg i = Message
  (Just $ runPut (putWord8 int64init))
  Nothing
  (Just $ runPut (putWord64be (fromIntegral i)))

-- >>> (isTinyInt 127 == True) && (isTinyInt 128 == False)
-- True
isTinyInt :: Integer -> Bool
isTinyInt i = i >= (-16) && i <= 127

-- >>> (isInt8 -18 == False) && (isInt8 -128 == True)
-- True
isInt8 :: Integer -> Bool
isInt8 i = i >= (-128) && i <= (-17)

-- >>> (isInt16 -32768 && isInt16 (7000) && not (isInt16 32769)
-- True
isInt16 :: Integer -> Bool
isInt16 i = i >= (-32768) && i <= 32768

isInt32 :: Integer -> Bool
isInt32 i = i >= (-int32Max) && i <= int32Max

-- | Is @__i__@ in the range of a packstream 64-bit Int type.
--
isInt64 :: Integer -> Bool
isInt64 i = (-int64Max) <= i && i <= int64Max

int32Max = 2147483648

int64Max = 9223372036854775808

-- Initialization bytes
int8init = 0xC8
int16init = 0xC9
int32init = 0xCA
int64init = 0xCB
tinyStringInit = 0x80
str8Init = 0xD0

{-
let MAX_CHUNK_SIZE = 16383,
TINY_STRING = 0x80,
TINY_LIST = 0x90,
TINY_MAP = 0xA0,
TINY_STRUCT = 0xB0,
NULL = 0xC0,
FLOAT_64 = 0xC1,
FALSE = 0xC2,
TRUE = 0xC3,
INT_8 = 0xC8,
INT_16 = 0xC9,
INT_32 = 0xCA,
INT_64 = 0xCB,
STRING_8 = 0xD0,
STRING_16 = 0xD1,
STRING_32 = 0xD2,
LIST_8 = 0xD4,
LIST_16 = 0xD5,
LIST_32 = 0xD6,
MAP_8 = 0xD8,
MAP_16 = 0xD9,
MAP_32 = 0xDA,
STRUCT_8 = 0xDC,
STRUCT_16 = 0xDD;
-}
