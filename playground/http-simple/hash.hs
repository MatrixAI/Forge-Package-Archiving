import Crypto.Hash (hashInitWith, SHA256(..), Digest(..), Context(..), hashUpdate)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit (yield, await, Source, Conduit, Sink, ($$), (=$))
import qualified Data.ByteString.Char8 as ByteS (ByteString, pack, length) 

--Initialise a hashing context
hContext = hashInitWith SHA256
source :: Source IO ByteS.ByteString
source = do
  yield $ ByteS.pack "a"
  yield $ ByteS.pack "ab"
  yield $ ByteS.pack "abc"

--conduit h ::  Conduit ByteS.ByteString IO (Context SHA256)
--conduit = do 
--  mbs <- await
--  case mbs of
--    Just bs -> do
--      yield $ (hashUpdate hContext bs)
--      conduit
--    Nothing -> return ()

sink :: (Context SHA256) -> Sink ByteS.ByteString IO ()
sink h = do
  mbs <- await
  case mbs of
    Just bs -> do
      context <- hashUpdate h bs
      sink context
    Nothing -> do
      digest <- hashFinalize h
      liftIO $ print $ show digest
      return () 

main = source =$ sink hContext
