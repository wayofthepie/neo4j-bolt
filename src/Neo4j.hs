module Neo4j where

import qualified Data.ByteString.Char8 as C
import Data.Word
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Safe

{-@ type Port = { p:Int | p > 0 && p <= 65535 } @-}

-- | The bolt versions this client supports.
supportedVersions :: [Word32]
supportedVersions = [1, 0, 0, 0]

-- | Try to connect to hostname at port.
{-@ connectTo :: String -> Port -> IO (Either String Socket) @-}
connectTo :: String -> Int -> IO (Either String Socket)
connectTo host port = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
  let serverAddr = headMay addrInfo
  maybe (pure $ Left "Not host found") 
        (\sa -> connectTo' sa >>= pure . Right) 
        serverAddr

connectTo' :: AddrInfo -> IO Socket
connectTo' serverAddr = do
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  pure sock

