import Data.Conduit.Binary (sinkFile) -- Exported from the package conduit-extra
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (runResourceT)

main :: IO ()
main = do
     request <- parseRequest "http://google.com/"
     manager <- newManager tlsManagerSettings
     runResourceT $ do
         response <- http request manager
         responseBody response C.$$+- sinkFile "google.html"
