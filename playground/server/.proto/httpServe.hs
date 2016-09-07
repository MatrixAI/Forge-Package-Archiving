module HttpServe (sendHTML, sendText) where

import Codec.Binary.UTF8.String (encodeString)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Network.HTTP.Server as Server 
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.URI
import System.FilePath
import System.IO
import Text.XHtml

import StreamHash

-- mkManagerWithChunkSize :: Int -> ManagerSettings
-- mkManagerWithChunkSize chunkSize = tlsManagerSettings {
--     managerRawConnection = fmap ($ chunkSize) $ rawConnectionModifySocketSize (const $ return ())
-- }

sendHTML       :: StatusCode -> Html -> Server.Response String
sendHTML s v    = insertHeader HdrContentType "text/html" $ sendText s (renderHtml v)

sendText       :: StatusCode -> String -> Server.Response String
sendText s v    = insertHeader HdrContentLength (show (length $ encodeString v))
                $ insertHeader HdrContentEncoding "UTF-8"
                $ insertHeader HdrContentEncoding "text/plain"
                $ (respond s :: Server.Response String) { rspBody = encodeString v }

main = serverWith defaultConfig 
    $ \_ _ request ->
        case rqMethod request of
            GET -> do
                let query = uriQuery $ rqURI request
                case query of
                    "" -> return $ sendText NotFound $ reason NotFound
                    _ -> do
                         
            _ -> do
                return $ sendText BadRequest $ reason BadRequest 
