import Crypto.Hash (hashInitWith, SHA256(..), Digest(..), Context(..), hashUpdate, hashFinalize)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit (yield, await, Source, Conduit, Sink, ($$), ($$+-))
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit (parseRequest, tlsManagerSettings, newManager, http, responseBody)
import Control.Monad.Trans.Resource (runResourceT, ResourceT(..))
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8 as ByteS (ByteString, pack) 

--download a binary file and produce a source
--
main = do
  request <- parseRequest "GET https://aur.archlinux.org/cgit/aur.git/snapshot/pacman-static.tar.gz"
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http request manager 
    -- responseBody response $$+- hashSink hContext 
    responseBody response $$+- sinkFile "test.tar.gz"
    liftIO $ print "Done"
    
--Initialise a hashing context
hContext = hashInitWith SHA256

--test stream some bytestrings as source to sink
testSource :: Source IO ByteS.ByteString
testSource = do
  yield $ ByteS.pack "a"
  yield $ ByteS.pack "ab"
  yield $ ByteS.pack "abc"

--receive bytestrings from upstream, and update hashing context
hashSink :: (Context SHA256) -> Sink ByteS.ByteString (ResourceT IO) ()
hashSink hc = do
  mbs <- await
  case mbs of
    Just bs -> do
      hashSink $ hashUpdate hc bs 
    Nothing -> do
      let digest = hashFinalize hc
      liftIO $ print $ show digest

-- digest = testSource $$ sink hContext

