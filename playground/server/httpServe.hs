--Server dependencies
import Network.HTTP.Server
import Network.HTTP.Headers
import Network.HTTP.Client
import System.FilePath
import Text.XHtml
import Codec.Binary.UTF8.String
import System.IO
import qualified Data.ByteString as ByteString

main :: IO ()

main = serverWith defaultConfig
  $ \_ url request ->
    --Append the HTTP reponse code
    do
        --Open a connection client manager to download files
        clientManager <- newManager defaultManagerSettings
        --make a test request
        getTest <- parseRequest "httpbin.org/get"
        withReponse getTest clientManager 
        $ \response ->
              do
                  body <- responseBody response
                  return $ brRead body

        --Return some text to the user
        return $ sendHTML OK
        --Construct body of response
        --Append </html> tag
          $ body (toHtml "Text, no matter what the request is")

sendHTML       :: StatusCode -> Html -> Response String
sendHTML s v    = insertHeader HdrContentType "text/html"
                $ sendText s (renderHtml v)

sendText       :: StatusCode -> String -> Response String
sendText s v    = insertHeader HdrContentLength (show (length txt))
                $ insertHeader HdrContentEncoding "UTF-8"
                $ insertHeader HdrContentEncoding "text/plain"
                $ (respond s :: Response String) { rspBody = txt }
  where txt       = encodeString v
