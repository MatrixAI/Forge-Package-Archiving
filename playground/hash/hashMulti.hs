{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

import Crypto.Hash (hashInitWith, HashAlgorithm, Digest(..), Context(..), hashUpdate, hashFinalize, 
  SHA256(..), MD5(..))

import Control.Monad.IO.Class (liftIO)
import Data.Conduit (yield, await, Source, Conduit, Sink, ($$+-), ($=+))
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit (parseRequest, tlsManagerSettings, newManager, http, responseBody)
import Control.Monad.Trans.Resource (runResourceT, ResourceT(..))

import Control.Concurrent (rtsSupportsBoundThreads, getNumCapabilities, setNumCapabilities)
import GHC.Conc (getNumProcessors) 
import qualified Data.ByteString.Char8 as ByteS (ByteString, pack, length) 

--import Control.Parallel.Strategies (parList, rdeepseq, using)
import Control.Parallel.Strategies (parMap, rpar)
import Control.Monad (when)

-- benchmarking library for timing
-- import Criterion.Main

-- quick data class to pack instances of context into
-- data HashesToBeComputed = ContextMD5 (Context MD5) | ContextSHA256 (Context SHA256)
data Contextable = forall h. (HashAlgorithm h) => Contextable (Context h) 
data Digestable = forall h. (HashAlgorithm h) => Digestable (Digest h)

--Initialise a hashing context
hSHA256 = Contextable $ hashInitWith SHA256 
hMD5 = Contextable $ hashInitWith MD5 
  
-- parallel map
--pmap f xs = map f xs `using` parList rdeepseq

main = do
--when rtsSupportsBoundThreads $ getNumProcessors >>= setNumCapabilities 
      
  streamMultiHash [hSHA256, hMD5] "http://mirror.internode.on.net/pub/test/100meg.test" "testFile" 

--download a binary file and produce a resumableSource
--streams at variable chunk sizes
streamMultiHash :: [Contextable] -> String -> FilePath -> IO ()
streamMultiHash contexts url file = do
  request <- parseRequest url 
  manager <- newManager tlsManagerSettings
  runResourceT $ do
    response <- http request manager 
    responseBody response $=+ hashC contexts $$+- sinkFile file 


--get bs from upstream, update context, yield bs downstream
--when all byte strings consumed, print the hash
hashC :: [Contextable] -> Conduit ByteS.ByteString (ResourceT IO) (ByteS.ByteString)
hashC !contexts = do
  mbs <- await
  case mbs of
    Just bs -> do 
      yield bs
      hashC $ map (\(Contextable hc) -> Contextable $! (hashUpdate hc bs)) contexts 
    Nothing -> do
      let digests = map (\(Contextable hc) -> Digestable (hashFinalize hc)) contexts 
      liftIO $ print $ map (\(Digestable d) -> show d) digests 

--test stream some bytestrings as source to sink
testSource :: Source IO ByteS.ByteString
testSource = do
  yield $ ByteS.pack "a"
  yield $ ByteS.pack "ab"
  yield $ ByteS.pack "abc"

