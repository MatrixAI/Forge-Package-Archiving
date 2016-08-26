{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

import Crypto.Hash (hashInitWith, HashAlgorithm, Digest(..), Context(..), hashUpdate, hashFinalize, 
  SHA256(..), MD5(..))

import Data.Conduit (yield, await, Source, Conduit, Sink, ($$+-), ($=+))
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit (parseRequest, tlsManagerSettings, newManager, http, responseBody)

import Control.Concurrent (rtsSupportsBoundThreads, getNumCapabilities, setNumCapabilities)
import GHC.Conc (getNumProcessors) 

import qualified Data.ByteString.Char8 as ByteS (ByteString, pack, length) 

import Control.Parallel.Strategies (parMap, rpar, rseq, rdeepseq)
import Control.DeepSeq (NFData(..), force)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT, ResourceT(..))

-- benchmarking library for timing
-- import Criterion.Main

-- quick data class to pack instances of context into
-- data HashesToBeComputed = ContextMD5 (Context MD5) | ContextSHA256 (Context SHA256)
data Contextable = forall h. (HashAlgorithm h) => Contextable (Context h) 
instance NFData Contextable where rnf (Contextable ctx) = rnf ctx 
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
hashC !ctxlist = do
  mbs <- await
  case mbs of
    Just bs -> do 
      yield bs
      hashC $ parMap rdeepseq (\(Contextable ctx) -> Contextable (hashUpdate ctx bs)) $ ctxlist
    Nothing -> do
      let digestable = map (\(Contextable ctx) -> Digestable (hashFinalize ctx)) $ ctxlist
      liftIO $ print $ map (\(Digestable digest) -> show digest) $ digestable

--test stream some bytestrings as source to sink
testSource :: Source IO ByteS.ByteString
testSource = do
  yield $ ByteS.pack "a"
  yield $ ByteS.pack "ab"
  yield $ ByteS.pack "abc"

