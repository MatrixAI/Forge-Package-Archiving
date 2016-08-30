import Control.Monad.IO.Class (liftIO)
import Data.Conduit (yield, Source, Conduit, Sink)
import qualified Data.ByteString as ByteS (ByteString, pack, length) 

--Initialise a hashing context
--hContext = hashInitWith SHA256

source :: Source IO ByteS.ByteString
source = do
  yield $ ByteS.pack "mystring1"
  yield $ ByteS.pack "mystring23"
  yield $ ByteS.pack "mystring323489y0uijlknazfsd"

conduit :: Conduit ByteS.ByteString IO Int 
conduit = do 
  mbs <- await
  case mbs of
    Just bs -> do
      yield $ ByteS.length bs
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

