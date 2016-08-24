import Crypto.Hash (hashInitWith, SHA256(..), Digest(..), Context(..), hashUpdate, hashFinalize)
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

conduit :: (Context SHA256) -> Conduit ByteS.ByteString IO (Digest SHA256)
conduit hc = do 
  mbs <- await
  case mbs of
    Just bs -> do
      conduit $ hashUpdate hc bs 
    Nothing -> do
      yield $ hashFinalize hc

sink :: Sink (Digest SHA256) IO ()
sink = do
  mDigest <- await
  case mDigest of
    Just digest -> do
      liftIO $ print $ show digest
    Nothing -> do
      liftIO $ print "No Digest"

main = source $$ conduit hContext =$ sink
