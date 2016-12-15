{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan (TBMChan(..))
import Control.Concurrent.STM.TChan (newBroadcastTChan, dupTChan)
import Control.Concurrent.STM.TMChan (dupTMChan, newBroadcastTMChan)
import Control.Concurrent.STM.TVar (TVar(..), newTVar)
import Control.DeepSeq (NFData(..), force)
import Control.Exception (bracket, catch, SomeException, throw)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (runResourceT, ResourceT(..))
import Control.Parallel.Strategies (parMap, rpar, rseq, rdeepseq)

import Crypto.Hash (hashInitWith, HashAlgorithm, Digest(..), Context(..), hashUpdate, hashFinalize, SHA256(..), MD5(..))

import Data.ByteString.Builder (byteString)
import Data.Conduit (unwrapResumable, yield, await, Conduit, ResumableSource, ($$), (=$=))
import Data.Conduit.Async (($$&), (=$=&))
import Data.Conduit.Binary (sinkFile)
import Data.Conduit.TMChan (sinkTBMChan, sourceTBMChan)
import Data.Maybe (fromMaybe)
import Data.Typeable

import Network.HTTP.Conduit (parseRequest, newManager, Manager(..), tlsManagerSettings, http, responseBody, Response(..), HttpException(..), HttpExceptionContent(..), closeManager)
import Network.HTTP.Types (status200, status404, status500)
import Network.Wai (Application(..), responseStream, responseLBS, queryString, requestMethod)
import Network.Wai.Handler.Warp (run, runSettings, Settings(..), defaultSettings, exceptionResponseForDebug, setOnExceptionResponse, setOnException)

import System.IO (stdout, stderr)

import qualified Data.ByteString.Char8 as BS (unpack, pack, length, ByteString(..))
import qualified Data.Conduit.List as CL
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

--need to pack types of (Context/Digest h) into a data type (Contextable/Digestable)
data Contextable = forall h. (HashAlgorithm h) => Contextable (Context h) 
instance NFData Contextable where rnf (Contextable ctx) = rnf ctx 
data Digestable = forall h. (HashAlgorithm h) => Digestable (Digest h)

--make a list of all the hashes we might need to produce from our stream, called "contexts"
contexts :: [Contextable]
contexts = [Contextable (hashInitWith SHA256), Contextable (hashInitWith MD5)]

--hashC: Conduit that, given a list of packed contexts, returns a conduit that will process a stream of bytestrings and return hash values to be written into multikey index
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

--newBroadcastTBMChan: broadcast bounded memory channel. Needed because functionality not included in STM Library
newBroadcastTBMChan :: Int -> STM (TBMChan a)
newBroadcastTBMChan n = do
    closed <- newTVar False
    slots <- newTVar n
    reads <- newTVar 0
    chan  <- newBroadcastTChan
    return (TBMChan closed slots reads chan)

dupTBMChan :: TBMChan a -> STM (TBMChan a)
dupTBMChan (TBMChan closed slots reads chan) = do
    new_chan <- dupTChan chan
    return (TBMChan closed slots reads new_chan)

app :: Application
app req respond = do
    case requestMethod req of
        methodGet -> do
            let mURL = fromMaybe Nothing $ lookup "url" $ queryString req
            case mURL of
                Nothing -> runResourceT $ do
                    liftIO $ respond $ responseLBS status404 [] $ "couldn't parse url"
                Just url -> runResourceT $ do

                    downloadRequest <- liftIO $ parseRequest $ BS.unpack url
                    downloadManager <- liftIO $ newManager tlsManagerSettings        
  
                    package <- http downloadRequest downloadManager 
                    (packageSource, _) <- unwrapResumable $ responseBody $ package
                   
                    --respondException :: SomeException -> Response
                    --this is used to send trailing headers in the event that the size of the response body
                    --does not match the hash of the response body or does not match the content size specified
                    --in the header of the upstream file transmission.
                    let respondException = \ex -> case ex of
                         HttpExceptionRequest _ (ResponseBodyTooShort _ _ ) -> ("the response body was too short") 
                         otherwise -> throw ex 

                    --responder :: Response
                    --responseStream :: Status -> ResponseHeaders -> StreamingBody -> Response
                    --StreamingBody :: (Builder -> IO ()) -> IO () -> IO ()
                    --Represents a streaming HTTP response body. It's a function of two parameters; 
                    --the first parameter provides a means of sending another chunk of data,
                    --and the second parameter provides a means of flushing the data to the client.
                    let responder = responseStream status200 [] $ \write flush -> runResourceT $ do
                        let sinkToClient = CL.mapM_ (\bs -> liftIO $ write (byteString bs) >> flush)

                        chan <- liftIO $ atomically $ newBroadcastTBMChan 16
                        chanR1 <- liftIO $ atomically $ dupTBMChan chan
                        chanR2 <- liftIO $ atomically $ dupTBMChan chan

                        --the underlying conduit which downloads the package and writes it into broadcast channel as a sink
                        sourceID <- liftIO . forkIO . runResourceT $ packageSource $$& sinkTBMChan chan True

                        --start two threaded conduits that are responsible for sending the package data to the client and hashing the client information
                        (_,_) <- liftIO $ concurrently 
                            (catch (runResourceT $ (sourceTBMChan chanR1) $$& sinkToClient)
                                (\e -> print ("streaming to client: " ++ show (e :: SomeException)) >> return ())
                            )
                            (catch (runResourceT $ (sourceTBMChan chanR2) =$=& hashC contexts $$& sinkFile "assets/testfile")
                                (\e -> print ("streaming to hash server: " ++ show (e :: SomeException)) >> return ())
                                )

                        return ()

                    liftIO $ respond $ responder

                       
                        -- TODO FURTHER: have the app handle routing, so that we have two modes, a query mode and a binary download mode, and permalinks
                        --packageSource $=+ hashC contexts $$+- sinkToClient
main :: IO ()
main = do
    runSettings defaultSettings app
