{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash (hashInitWith, SHA256(..), MD5(..))

import Control.Exception (bracket)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Resource (runResourceT)

import Data.ByteString.Builder (stringUtf8, byteString)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Char8 (unpack)
import Data.Maybe (fromMaybe)
import Data.Conduit (await, yield, Flush(..), unwrapResumable, Conduit, (=$=))
-- import Network.HTTP.Client (ManagerSettings(..), rawConnectionModifySocketSize,)
import Network.HTTP.Conduit (parseRequest, newManager, tlsManagerSettings, http, responseBody)
import Network.Wai (Application(..), queryString, rawQueryString, requestMethod, responseBuilder, responseLBS, responseHeaders)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Conduit (sourceRequestBody, responseSource, responseRawSource)
import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.Types.Status (status200, status404)
import Network.HTTP.Types.URI (Query(..))

import StreamHash
import Data.Typeable

-- mkManagerWithChunkSize :: Int -> ManagerSettings
-- mkManagerWithChunkSize chunkSize = tlsManagerSettings {
--     managerRawConnection = fmap ($ chunkSize) $ rawConnectionModifySocketSize (const $ return ())
-- }

-- serverApp :: Application
-- serverApp req doResponse = do
--     case requestMethod req of
--         methodGet -> do
--             let mURL = fromMaybe Nothing $ lookup "url" $ queryString req
--             case mURL of 
--                 Nothing -> doResponse $ responseLBS status404 [] $ "No url found in query string"
--                 Just url -> do
--                     -- let settings = mkManagerWithChunkSize 8192 
--                     -- let hashes = [Contextable (hashInitWith SHA256), Contextable (hashInitWith MD5)]
--                     let stringURL = unpack url
--                     -- streamHash settings hashes stringURL "out" 
--                     dmgr <- newManager tlsManagerSettings
--                     pkgreq <- parseRequest stringURL
--                     response <- http request manager
--                     let sourceReq = sourceRequestBody req                  
--                     doResponse $ responseLBS status404 [] $ "No url found in query string"


byteStringToBuilder = do 
     mbs <- await
     case mbs of
         Just bs -> do
             yield $ Chunk $ byteString $ bs
             byteStringToBuilder
         Nothing -> do
             yield $ Flush

serverApp :: Application
serverApp req responder = do
    pkgreq <- parseRequest "http://httpbin.org/get"
    print req
    dmgr <- newManager tlsManagerSettings

    runResourceT $ do
        pkg <- http pkgreq dmgr
        (packageSource, _) <- unwrapResumable $ responseBody $ pkg
        liftIO $ print $ typeOf packageSource
        -- liftIO $ responder $ responseSource status200 [] (reqSource =$= byteStringToBuilder) 
        liftIO $ responder $ responseSource status200 [] (yield $ Chunk $ stringUtf8 $ "Hello")

main :: IO ()
main = do
    run 8000 serverApp
