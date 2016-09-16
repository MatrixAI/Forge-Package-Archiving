{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Conduit.List as CL

import Data.ByteString.Builder (byteString)
import Data.Conduit (await, yield, Flush(..), unwrapResumable, Conduit, (=$=), ($$))
-- import Network.HTTP.Client (ManagerSettings(..), rawConnectionModifySocketSize,)
import Network.HTTP.Conduit (parseRequest, newManager, tlsManagerSettings, http, responseBody)
import Network.Wai (Application(..), responseStream)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Method (methodGet)
import Network.HTTP.Types.Status (status200, status404)

app :: Application
app _ respond = respond $ responseStream status200 [] $ \write flush -> runResourceT $ do
    downloadRequest <- liftIO $ parseRequest "http://httpbin.org/get"
    downloadManager <- liftIO $ newManager tlsManagerSettings        
    -- http :: Request -> Manager -> Response (ResumableSource) 
    package <- http downloadRequest downloadManager
    -- we discard the finaliser, because we are already inside a ResourceT transformer
    (packageSource, _) <- unwrapResumable $ responseBody package

    let sinkToClient = CL.mapM_ (\bs -> liftIO $ write (byteString bs) >> flush)
    packageSource $$ sinkToClient

main :: IO ()
main = do
    run 8000 app
