{-# LANGUAGE BangPatterns #-}

import Crypto.Hash (hashInitWith, SHA256(..), Digest(..), Context(..), hashUpdate, hashFinalize)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit (yield, await, Source, Conduit, Sink, ($$+-), ($=+))
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit (parseRequest, tlsManagerSettings, newManager, http, responseBody)
import Control.Monad.Trans.Resource (runResourceT, ResourceT(..))
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8 as ByteS (ByteString, pack, length) 


-- benchmarking library for timing
-- import Criterion.Main

--Initialise a hashing context
hContext = hashInitWith SHA256

main = streamHash "http://releases.nixos.org/nixos/16.03/nixos-16.03.1171.9cb194c/nixos-graphical-16.03.1171.9cb194c-x86_64-linux.iso" 

--download a binary file and produce a resumableSource
--streams at variable chunk sizes
streamHash :: String -> IO ()
streamHash url = do
  request <- parseRequest url 
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http request manager 
    responseBody response $$+- hashSink hContext

--receive bytestrings from upstream, and update hashing context
--when done, print the hash
hashConduit :: (Context SHA256) -> Sink ByteS.ByteString (ResourceT IO) (ByteS.ByteString) 
hashSink !hc = do
  mbs <- await
  case mbs of
    Just bs -> do
      liftIO $ print $ show $ ByteS.length bs
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

