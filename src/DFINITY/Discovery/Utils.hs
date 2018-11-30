--------------------------------------------------------------------------------

-- |
-- Module:      DFINITY.Discovery.Utils
-- Description: Utility functions and types for @dfinity-discovery@
--
-- FIXME: doc

--------------------------------------------------------------------------------

module DFINITY.Discovery.Utils
  ( threadDelay
  , hour
  , minute
  ) where

--------------------------------------------------------------------------------

import qualified Control.Concurrent (threadDelay)

--------------------------------------------------------------------------------

-- |
-- Delay the current thread for the given number of seconds.
threadDelay :: Int -> IO ()
threadDelay n
  | (n < 0)   = error "DFINITY.Discovery.Utils.threadDelay: negative number!"
  | otherwise = Control.Concurrent.threadDelay (n * 1000000)

--------------------------------------------------------------------------------

-- |
-- Given a number of hours, return the number of seconds.
--
-- For example, @hour 5 ≡ 18000@, and there are 18000 seconds in 5 hours.
hour :: Num a => a -> a
hour n = 3600 * n

--------------------------------------------------------------------------------

-- |
-- Given a number of minutes, return the number of seconds.
--
-- For example, @minute 5 ≡ 300@, and there are 300 seconds in 5 minutes.
minute :: Num a => a -> a
minute n = 60 * n

--------------------------------------------------------------------------------
