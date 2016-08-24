{-# LANGUAGE BangPatterns #-}

import Crypto.Hash (hashInitWith, HashAlgorithm, Digest(..), Context(..), hashUpdate, hashFinalize, 
  SHA256(..))

import Control.Monad.IO.Class (liftIO)
import Data.Conduit (yield, await, Source, Conduit, Sink, ($$+-), ($=+))
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit (parseRequest, tlsManagerSettings, newManager, http, responseBody)
import Control.Monad.Trans.Resource (runResourceT, ResourceT(..))

import qualified Data.ByteString.Char8 as ByteS (ByteString, pack, length) 


-- benchmarking library for timing
-- import Criterion.Main

--Initialise a hashing context
hSHA256 = hashInitWith SHA256

main = do
  streamHash "http://mirror.internode.on.net/pub/test/10meg.test"

--download a binary file and produce a resumableSource
--streams at variable chunk sizes
streamHash :: String -> IO ()
streamHash url = do
  request <- parseRequest url 
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http request manager 
    responseBody response $$+- hashSink hSHA256

--receive bytestrings from upstream, and update hashing context
--when all byte strings consumed, print the hash
hashSink :: (HashAlgorithm h) => Context h -> Sink ByteS.ByteString (ResourceT IO) ()
hashSink !hc = do
  mbs <- await
  case mbs of
    Just bs -> do 
      let context = hashUpdate hc bs
      hashSink context
    Nothing -> do
      let digest = hashFinalize hc 
      liftIO $ print $ show digest

--test stream some bytestrings as source to sink
testSource :: Source IO ByteS.ByteString
testSource = do
  yield $ ByteS.pack "a"
  yield $ ByteS.pack "ab"
  yield $ ByteS.pack "abc"

