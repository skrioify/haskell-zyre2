-- | Provides configuration functions to handle setting up non-default
-- options and toggles.
module Network.Zyre2.Configuration where

import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (free)
import Network.Zyre2.Bindings
  ( zyreSetEvasiveTimeout,
    zyreSetExpiredTimeout,
    zyreSetHeader,
    zyreSetInterface,
    zyreSetInterval,
    zyreSetName,
    zyreSetPort,
    zyreSetSilentTimeout,
    zyreSetVerbose,
  )
import Network.Zyre2.Types (ZCreated, ZyreContext (ZyreContext), unlessStale)

-- | Set the name of the context.
setName :: ZyreContext ZCreated -> Text -> IO ()
setName zctx@(ZyreContext ptr _ _) name = unlessStale zctx $ do
  cname <- newCString (T.unpack name)
  zyreSetName ptr cname
  free cname

-- | Set a header value. Headers are sent with every 'Enter' message.
setHeader :: ZyreContext ZCreated -> Text -> Text -> IO ()
setHeader zctx@(ZyreContext ptr _ _) headerName headerValue = unlessStale zctx $ do
  cheaderName <- newCString (T.unpack headerName)
  cheaderValue <- newCString (T.unpack headerValue)
  zyreSetHeader ptr cheaderName cheaderValue
  free cheaderName
  free cheaderValue

-- | Enable verbose mode, logging most actions zyre does.
setVerbose :: ZyreContext ZCreated -> IO ()
setVerbose zctx@(ZyreContext ptr _ _) = unlessStale zctx $ do
  zyreSetVerbose ptr

-- | Set a specific port that zyre uses. By default zyre uses an ephemereal port.
setPort :: ZyreContext ZCreated -> Int -> IO ()
setPort zctx@(ZyreContext ptr _ _) port = unlessStale zctx $ do
  zyreSetPort ptr (fromIntegral port)

-- | Set the time in milliseconds for a node to be considered evasive.
-- Default is 5000.
setEvasiveTimeout :: ZyreContext ZCreated -> Int -> IO ()
setEvasiveTimeout zctx@(ZyreContext ptr _ _) timeout = unlessStale zctx $ do
  zyreSetEvasiveTimeout ptr (fromIntegral timeout)

-- | Set the time in milliseconds for a node to be considered silent.
-- Default is 5000.
setSilentTimeout :: ZyreContext ZCreated -> Int -> IO ()
setSilentTimeout zctx@(ZyreContext ptr _ _) timeout = unlessStale zctx $ do
  zyreSetSilentTimeout ptr (fromIntegral timeout)

-- | Set the time in milliseconds for a node to be considered expired.
-- Default is 30000.
setExpiredTimeout :: ZyreContext ZCreated -> Int -> IO ()
setExpiredTimeout zctx@(ZyreContext ptr _ _) timeout = unlessStale zctx $ do
  zyreSetExpiredTimeout ptr (fromIntegral timeout)

-- | Set the UDP beaconing interval. A node will instantly beacon on
-- connecting, regardless of interval.
-- Default is 1000.
setInterval :: ZyreContext ZCreated -> Int -> IO ()
setInterval zctx@(ZyreContext ptr _ _) interval = unlessStale zctx $ do
  zyreSetInterval ptr (fromIntegral interval)

-- | Set network interface for UDP beacons. If you do not set this, CZMQ will
-- choose an interface for you. On boxes with several interfaces you should
-- specify which one you want to use, or strange things can happen.
setInterface :: ZyreContext ZCreated -> Text -> IO ()
setInterface zctx@(ZyreContext ptr _ _) interface = unlessStale zctx $ do
  cinterface <- newCString (T.unpack interface)
  zyreSetInterface ptr cinterface
  free cinterface