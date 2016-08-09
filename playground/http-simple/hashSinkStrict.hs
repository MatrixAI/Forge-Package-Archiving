{-# LANGUAGE BangPatterns #-}

import Crypto.Hash (hashInitWith, SHA256(..), Digest(..), Context(..), hashUpdate, hashFinalize)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit (yield, await, Source, Conduit, Sink, ($$+-), ($=+))
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit (parseRequest, tlsManagerSettings, newManager, http, responseBody, responseHeaders)
import Control.Monad.Trans.Resource (runResourceT, ResourceT(..))
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8 as ByteS (ByteString, pack, length) 


-- benchmarking library for timing
-- import Criterion.Main

--Initialise a hashing context
hContext = hashInitWith SHA256

main = do
  digest <- streamHash "http://mirror.internode.on.net/pub/test/10meg.test"
  print digest

--download a binary file and produce a resumableSource
--streams at variable chunk sizes
streamHash :: String -> IO (String)
streamHash url = do
  request <- parseRequest url 
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http request manager 
    let head = responseHeaders response 
    let first = head !! 1
    liftIO $ print $ show $ fst first
    liftIO $ print $ show $ snd first
    digestString <- responseBody response $$+- hashSink hContext
    liftIO $ return digestString

--receive bytestrings from upstream, and update hashing context
--when done, print the hash
hashSink :: (Context SHA256) -> Sink ByteS.ByteString (ResourceT IO) (String)
hashSink !hc = do
  mbs <- await
  case mbs of
    Just bs -> do
      let context = hashUpdate hc bs
      hashSink context
    Nothing -> do
      let digest = hashFinalize hc 
      return $ show digest

--test stream some bytestrings as source to sink
testSource :: Source IO ByteS.ByteString
testSource = do
  yield $ ByteS.pack "a"
  yield $ ByteS.pack "ab"
  yield $ ByteS.pack "abc"

