module Neo4j where

import qualified Data.ByteString.Char8 as C
import Control.Monad (foldM)
import Control.Monad.IO.Class
import Data.Bits
import Data.Either (either)
import Data.List (foldl')
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Word
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Safe
import PackStream.Internal.Message

import Prelude hiding (head, init, tail)

{-@ type Port = { p:Int | p > 0 && p <= 65535 } @-}

{-@ predicate NonNull X = ((len X) > 0) @-}

{-@ head   :: {v:[a] | (NonNull v)} -> a @-}
head (x:_) = x
head []    = impossible "Can never happen."

{-@ tail :: {v:[a] | (NonNull v)} -> [a] @-}
tail (_:xs) = xs
tail []     = impossible "Can never happen."

{-@ impossible :: {v:String | false} -> a  @-}
impossible msg = error msg


-- | Must be sent in the handshake before version negotiation.
magicPreamble :: Word32
magicPreamble = 0x6060B017

magicPreambleBS :: C.ByteString
magicPreambleBS = runPut (putWord32be magicPreamble)

-- | The bolt versions this client supports.
{-@ supportedVersions :: { v:[Word32] | NonNull v } @-}
supportedVersions = [1, 0, 0, 0]

supportedVersionsBS :: C.ByteString
supportedVersionsBS = runPut $
    foldl (\acc ver -> acc <* putWord32be ver)
      (putWord32be $ head supportedVersions)
    (tail supportedVersions)

init :: Word32
init = 0x01

ackFailure = 0x0F
userAgent = "Haskell Neo4j Bolt/0.0.0.1"

-- | Connect to server and run the handshake.
{-@ initConnection :: String -> Port -> IO (Either String Socket) @-}
initConnection hostname port = do
  eitherSocket <- connectTo hostname port
  either (pure . Left) handShake eitherSocket
 where
  handShake :: Socket -> IO (Either String Socket)
  handShake s = do
    sendAll s magicPreambleBS
    sendAll s supportedVersionsBS
    bs <- recv s 4
    either (pure . Left)
           (\_ -> sendInitUA s >>= pure . Right)
           (verifyServerVersion bs)

  sendInitUA s = do
    sendAll s (runPut $ putWord32be init)
    sendAll s (runPut $ putWord32be 0x00)
    sendAll s (runPut $ putByteString (C.pack userAgent))
    sendAll s (runPut $ putWord32be 0x00)
    pure s

-- | Verify the version returned from the server is valid.
-- FIXME : We need to check it matches our supported versions.
verifyServerVersion :: C.ByteString -> Either String Bool
verifyServerVersion bs =
  either (\_ -> Left "No protocol version could be agreed")
         (pure . ((==) 0) )
         (runGet getWord32be bs)

-- | Try to connect to hostname at port.
{-@ connectTo :: String -> Port -> IO (Either String Socket) @-}
connectTo :: String -> Int -> IO (Either String Socket)
connectTo host port = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serverAddr = headMay addrInfo
  maybe (pure $ Left "No host found")
        (\sa -> connectTo' sa >>= pure . Right)
        serverAddr
 where
  connectTo' :: AddrInfo -> IO Socket
  connectTo' serverAddr = do
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    pure sock

-- | Convert a Word32 to a list of Word8's.
octets :: Word32 -> [Word8]
octets w =
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]
fromOctets :: [Word8] -> Word32
fromOctets = foldl' accum 0
 where
  accum a o = (a `shiftL` 8) .|. fromIntegral o
