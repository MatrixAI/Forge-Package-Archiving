import Crypto.Hash
import Data.ByteString.Char8 as C 
import Crypto.Hash.Algorithms
import Control.Monad.IO.Class
import Data.Conduit.Binary as B
import Data.Conduit
import Control.Monad.Trans.Resource

--Initialise a hashing context
hContext = hashInitWith SHA256

source :: Source IO ByteString
source = do
  yield $ pack "mystring1"
  yield $ pack "mystring23"
  yield $ pack "mystring323489y0uijlknazfsd"

conduit :: Conduit ByteString IO Int 
conduit = do 
  mbs <- await
  case mbs of
    Just bs -> do
      yield $ C.length bs
      conduit
    Nothing -> return ()

sink :: Int -> Sink Int IO () 
sink c = do
  mInt <- await
  case mInt of
    Just int -> do
      sink (int + c)
      liftIO $ print (int + c)
    Nothing -> liftIO $ print c

main = source $$ conduit =$ sink 0

