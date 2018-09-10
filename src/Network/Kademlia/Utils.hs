module Network.Kademlia.Utils
       ( threadDelay
       , hour
       , minute
       ) where

import           Control.Monad.Conc.Class (MonadConc)
import qualified Control.Monad.Conc.Class (threadDelay)

-- thread delay in seconds
threadDelay :: (MonadConc m) => Int -> m ()
threadDelay n = Control.Monad.Conc.Class.threadDelay (n * 1000000)

hour, minute :: Num a => a -> a
hour   n = 3600 * n
minute n = 60   * n
