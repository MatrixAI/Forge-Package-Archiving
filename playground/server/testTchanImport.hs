import Control.Concurrent.STM.TBMChan 
import Control.Concurrent.STM (STM(..))
import Control.Concurrent.STM.TVar (TVar(..), newTVar)
import Control.Concurrent.STM.TChan (newBroadcastTChan)

newBroadcastTBMChan :: Int -> STM (TBMChan a)
newBroadcastTBMChan n = do
    closed <- newTVar False
    slots <- newTVar n
    reads <- newTVar 0
    chan  <- newBroadcastTChan
    return (TBMChan closed slots reads chan)
