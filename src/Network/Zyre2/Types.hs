{-# LANGUAGE DeriveDataTypeable #-}

-- | Holds some types that the modules requires in common.
module Network.Zyre2.Types where

import Control.Exception (Exception, throw)
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Foreign (Ptr)

-- | Runtime exceptions that can be thrown.
data ZyreException
  = StaleZyreContextException
  | ZyreMsgDontSupportFramesException
  deriving (Show, Typeable)

instance Exception ZyreException

-- Context to apply to zyre functions.

-- | A context handle for zyre contexts. Holds relevant state for
-- the context, and is tagged with the state of the context.
-- E.g. 'ZCreated', 'ZRunning', 'ZStopped', 'ZDestroyed'.
data ZyreContext state = ZyreContext
  { _zyreContextPtr :: Ptr (),
    _zyreContextStale :: IORef Bool,
    -- | Mapping of node-ids to node names.
    _zyreContextNodeNames :: IORef (Map.Map Text Text)
  }

-- Phantom tags for the zyre context.

-- | Phantom tag for a created context.
data ZCreated = ZCreated

-- | Phantom tag for a running context.
data ZRunning = ZRunning

-- | Phantom tag for a stopped context.
data ZStopped = ZStopped

-- | Phantom tag for a destroyed context.
data ZDestroyed = ZDestroyed

-- | Perform an IO a action unless the context is stale, then throw
-- a 'StaleZyreContextException' instead.
unlessStale :: ZyreContext s -> IO a -> IO a
unlessStale (ZyreContext _ stale _) fn = do
  isStale <- readIORef stale
  if isStale
    then throw StaleZyreContextException
    else fn