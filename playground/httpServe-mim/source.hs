import Control.Monad.IO.Class
import Data.Conduit

source :: Source IO String

source = do
    yield "1"
    yield "2"
    yield "3"
    yield "4"
    
conduit :: Conduit String IO (Int,Int)
conduit = do
    -- Get all of the adjacent pairs from the stream
    mi1 <- await
    mi2 <- await
    case (mi1, mi2) of
        (Just s1, Just s2) -> do
            yield $ (read ("(" ++ s1 ++ "," ++ s2 ++ ")") :: (Int,Int))
            leftover s2
            conduit
        _ -> return ()
            
sink :: Sink (Int,Int) IO ()
sink = do
    mIntTuple <- await
    case mIntTuple of
        Nothing -> return ()
        Just intTuple -> do
            liftIO $ putStrLn (show intTuple)
            sink
            
main = source $$ (conduit =$ sink) 
