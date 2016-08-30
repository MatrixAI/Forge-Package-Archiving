import Control.Monad.IO.Class (liftIO)
import Crypto.Hash (Digest(..), SHA256)
import Crypto.Hash.Conduit (sinkHash)
import Data.Conduit (yield, await, Consumer, Source, Conduit, Sink, ($$+-), ($=+))
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit (parseRequest, tlsManagerSettings, newManager, http, responseBody)

import Control.Monad.Trans.Resource (runResourceT, ResourceT(..))
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8 as ByteS (ByteString, pack, length) 

-- benchmarking library for timing
-- import Criterion.Main

main = streamHash "http://releases.nixos.org/nixos/16.03/nixos-16.03.1171.9cb194c/nixos-graphical-16.03.1171.9cb194c-x86_64-linux.iso"

--download a binary file and produce a resumableSource
--streams at variable chunk sizes
streamHash :: String -> IO ()
streamHash url = do
  request <- parseRequest url 
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http request manager 
    digest <- responseBody response $=+ conduit $$+- (sinkHash :: Consumer ByteS.ByteString (ResourceT IO) (Digest SHA256))
    liftIO $ print $ show digest

conduit :: Conduit (ByteS.ByteString) (ResourceT IO) (ByteS.ByteString)
conduit = do
  mbs <- await
  case mbs of
    Just bs -> do
      liftIO $ print $ show $ ByteS.length bs
      liftIO $ getChar
      yield bs
      conduit
    Nothing -> do
      liftIO $ print "Done"

testSource :: Source IO ByteS.ByteString
testSource = do
  yield $ ByteS.pack "a"
  yield $ ByteS.pack "ab"
  yield $ ByteS.pack "abc"

