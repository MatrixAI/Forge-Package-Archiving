import Crypto.Hash (hashInitWith, SHA256(..), Digest(..), Context(..), hashUpdate, hashFinalize)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit (yield, await, Source, Conduit, Sink, ($$))
import Network.HTTP.Conduit (parseRequest, tlsManagerSettings, newManager, http)
import qualified Data.ByteString.Char8 as ByteS (ByteString, pack) 

--download a binary file and produce a source
--
main = do
  request <- parseRequest "GET https://aur.archlinux.org/cgit/aur.git/snapshot/pacman-static.tar.gz"
  manager <- newManager tlsManagerSettings
  response <- http request manager
    
  print "Done"
--Initialise a hashing context
hContext = hashInitWith SHA256

--test stream some bytestrings as source to sink
testSource :: Source IO ByteS.ByteString
testSource = do
  yield $ ByteS.pack "a"
  yield $ ByteS.pack "ab"
  yield $ ByteS.pack "abc"

--receive bytestrings from upstream, and update hashing context
sink :: (Context SHA256) -> Sink ByteS.ByteString IO (Digest SHA256)
sink hc = do
  mbs <- await
  case mbs of
    Just bs -> do
      sink $ hashUpdate hc bs 
    Nothing -> do
      return $ hashFinalize hc

-- digest = testSource $$ sink hContext

