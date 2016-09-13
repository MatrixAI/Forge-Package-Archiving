{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module StreamHash (streamHash, Contextable(..), Digestable(..)) where

import Control.DeepSeq (NFData(..), force)
import Control.Exception (catch)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT, ResourceT(..))
import Control.Parallel.Strategies (parMap, rpar, rseq, rdeepseq)

import Crypto.Hash (hashInitWith, HashAlgorithm, Digest(..), Context(..), hashUpdate, hashFinalize, 
  SHA256(..), MD5(..))

import qualified Data.ByteString.Char8 as ByteS (ByteString, pack, length) 
import Data.Conduit (yield, await, Source, Conduit, Sink, ($$+-), ($=+))
import Data.Conduit.Binary (sinkFile)

import GHC.Conc (getNumProcessors) 

import Network.HTTP.Client (ManagerSettings(..), rawConnectionModifySocketSize)
import Network.HTTP.Conduit (parseRequest, newManager, tlsManagerSettings, http, responseBody)

import System.Environment (getArgs)

-- quick data class to pack instances of context and digest into
data Contextable = forall h. (HashAlgorithm h) => Contextable (Context h) 
instance NFData Contextable where rnf (Contextable ctx) = rnf ctx 
data Digestable = forall h. (HashAlgorithm h) => Digestable (Digest h)

--download a binary file and produce a resumableSource
--streams at variable chunk sizes
streamHash :: ManagerSettings -> [Contextable] -> String -> FilePath -> IO ()
streamHash managerSettings contexts url file = do
  manager <- newManager managerSettings
  request <- parseRequest url 
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

