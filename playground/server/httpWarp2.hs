{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as BS (unpack, pack, length, ByteString(..))
import Data.Conduit (($$), unwrapResumable, yield, await, ($$+-), ($=+), Conduit, ResumableSource)
import Data.Conduit.Binary (sinkFile)
import qualified Data.Conduit.List as CL
import Data.Maybe (fromMaybe)
import Data.Typeable
import Network.HTTP.Conduit (parseRequest, newManager, Manager(..), tlsManagerSettings, http, responseBody, Response(..))
import Network.Wai (Application(..), responseStream, responseLBS, queryString, requestMethod)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Control.DeepSeq (NFData(..), force)
import Control.Exception (catch)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT, ResourceT(..))
import Control.Parallel.Strategies (parMap, rpar, rseq, rdeepseq)
import Crypto.Hash (hashInitWith, HashAlgorithm, Digest(..), Context(..), hashUpdate, hashFinalize, 
  SHA256(..), MD5(..))

-- quick data class to pack instances of context and digest into
data Contextable = forall h. (HashAlgorithm h) => Contextable (Context h) 
instance NFData Contextable where rnf (Contextable ctx) = rnf ctx 
data Digestable = forall h. (HashAlgorithm h) => Digestable (Digest h)

contexts :: [Contextable]
contexts = [Contextable (hashInitWith SHA256), Contextable (hashInitWith MD5)]

--download a binary file and produce a resumableSource
--streams at variable chunk sizes
-- streamHash :: ResumableSource (ResourceT IO) BS.ByteString -> [Contextable] -> FilePath -> IO ()
-- streamHash src contexts out = do
--     runResourceT $ do
--         src $=+ hashC contexts $$+- sinkFile out 

--get bs from upstream, update context, yield bs downstream
--when all byte strings consumed, print the hash
hashC :: [Contextable] -> Conduit BS.ByteString (ResourceT IO) (BS.ByteString)
hashC !ctxlist = do
  mbs <- await
  case mbs of
    Just bs -> do 
      yield bs
      hashC $ parMap rdeepseq (\(Contextable ctx) -> Contextable (hashUpdate ctx bs)) $ ctxlist
    Nothing -> do
      let digestable = map (\(Contextable ctx) -> Digestable (hashFinalize ctx)) $ ctxlist

      -- TODO: instead of printing hash values, write hash info to multikey index along with file information
      liftIO $ print $ map (\(Digestable digest) -> show digest) $ digestable


app :: Application
app req respond = do
    case requestMethod req of
        methodGet -> do
            let mURL = fromMaybe Nothing $ lookup "url" $ queryString req
            case mURL of
                Nothing -> respond $ responseLBS status404 [] $ "No url found in query string"
                Just url -> do
                    respond $ responseStream status200 [] $ \write flush -> runResourceT $ do
                        
                        downloadRequest <- liftIO $ parseRequest $ BS.unpack url
                        downloadManager <- liftIO $ newManager tlsManagerSettings        
                        package <- http downloadRequest downloadManager
                        let packageSource = responseBody $ package
--                        let packageSource = (http downloadRequest downloadManager) >>= responseBody

                        let sinkToClient = CL.mapM_ (\bs -> liftIO $ write (byteString bs) >> flush)
                        -- TODO: write the binary file to server first, then IPFS, concurrently as you are hashing and feeding the output from the source back to the client
                        -- TODO FURTHER: have the app handle routing, so that we have two modes, a query mode and a binary download mode, and permalinks
                        packageSource $=+ hashC contexts $$+- sinkToClient

main :: IO ()
main = do
    run 8000 app
