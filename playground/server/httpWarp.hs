{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Lazy (ByteString(..), fromStrict)
import Network.HTTP.Client (ManagerSettings(..), rawConnectionModifySocketSize)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai (Application(..), rawQueryString, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.Types.Status (status200)

import StreamHash

mkManagerWithChunkSize :: Int -> ManagerSettings
mkManagerWithChunkSize chunkSize = tlsManagerSettings {
    managerRawConnection = fmap ($ chunkSize) $ rawConnectionModifySocketSize (const $ return ())
}

serverApp :: Application
serverApp req doResponse = do
    case requestMethod req of
        methodGet -> do
            let response = responseLBS status200 [] $ fromStrict $ rawQueryString $ req 
            doResponse response

main :: IO ()
main = do
    run 8000 serverApp
