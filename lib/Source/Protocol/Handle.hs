module Source.Protocol.Handle
  ( hGetMessage
  , hPutMessage
  , initHandle
  , SerializationError(..)
  ) where

import System.IO
import Data.ByteString as ByteString
import Data.Word
import Data.Serialize as Cereal
import Data.Monoid
import Control.Exception (Exception, throw, throwIO, assert)
import Data.Coerce

initHandle :: Handle -> IO ()
initHandle handle = do
  hSetBuffering handle NoBuffering
  hSetBinaryMode handle True

data SerializationError
  = SerializationError String
  | InvalidMessageLength
  deriving (Eq, Show)

instance Exception SerializationError

-- | The message length is stored as a 32-bit unsigned integer. This is enough
-- to support up to messages of size almost up to 4 GiB.
newtype MessageLength = MessageLength Word32
  deriving (Bounded)

-- | Big-endian encoding is used as it is common in networking.
instance Serialize MessageLength where
  put = coerce Cereal.putWord32be
  get = coerce Cereal.getWord32be

-- | Convert an 'Int' to 'MessageLength' with the necessary bound checking,
-- throws a 'SerializationError' in case of a failure.
toMessageLength :: Int -> MessageLength
toMessageLength n
  | outOfBounds = throw InvalidMessageLength
  | otherwise   = MessageLength (fromIntegral n)
  where
    minMessageLength = case minBound of MessageLength n' -> fromIntegral n'
    maxMessageLength = case maxBound of MessageLength n' -> fromIntegral n'
    outOfBounds =
      n < minMessageLength ||
      n > maxMessageLength

-- | An inverse of 'toMessageLength'.
fromMessageLength :: MessageLength -> Int
fromMessageLength (MessageLength n) = fromIntegral n

messageLengthBytes :: Int
messageLengthBytes =
  -- 4 bytes = 32 bits (to store Word32)
  4

hGetMessageLength :: Handle -> IO MessageLength
hGetMessageLength handle = do
  bs <- ByteString.hGet handle messageLengthBytes
  case Cereal.runGet Cereal.get bs of
    Left  e -> throwIO $ SerializationError e
    Right n -> return n

messageLengthToByteString :: MessageLength -> ByteString
messageLengthToByteString (MessageLength n) =
  let bs = Cereal.runPut (Cereal.put n)
  in assert (ByteString.length bs == messageLengthBytes) bs

hGetMessage :: Serialize message => Handle -> IO message
hGetMessage handle = do
  n <- hGetMessageLength handle
  bs <- ByteString.hGet handle (fromMessageLength n)
  case Cereal.runGet Cereal.get bs of
    Left  e -> throwIO $ SerializationError e
    Right a -> return a

hPutMessage :: Serialize message => Handle -> message -> IO ()
hPutMessage handle message = ByteString.hPut handle bsWithLen
  where
    bs = Cereal.runPut (Cereal.put message)
    bsWithLen = messageLengthToByteString messageLength <> bs
    messageLength = toMessageLength (ByteString.length bs)
