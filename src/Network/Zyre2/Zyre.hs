-- | Zyre provides reliable group messaging over local area networks. It has these key characteristics:
--
-- * Zyre needs no administration or configuration.
-- * Peers may join and leave the network at any time.
-- * Peers talk to each other without any central brokers or servers.
-- * Peers can talk directly to each other.
-- * Peers can join groups, and then talk to groups.
-- * Zyre is reliable, and loses no messages even when the network is heavily loaded.
-- * Zyre is fast and has low latency, requiring no consensus protocols.
-- * Zyre is designed for WiFi networks, yet also works well on Ethernet networks.
-- * Time for a new peer to join a network is about one second.
--
-- Typical use cases for Zyre are:
--
-- * Local service discovery.
-- * Clustering of a set of services on the same Ethernet network.
-- * Controlling a network of smart devices (Internet of Things).
-- * Multi-user mobile applications (like smart classrooms).
--
-- This package provides a haskell interface to the Zyre 2.0 API. The
-- package requires the c libraries czmq and zyre to be installed on the
-- system. See https://github.com/zeromq/zyre for specifics.
module Network.Zyre2.Zyre
  ( name,
    new,
    start,
    stop,
    destroy,
    join,
    leave,
    uuid,
    version,
    shout,
    shouts,
    whisper,
    whispers,
    recv,
    peers,
    peerAddress,
    peerHeaderValue,
    peersByGroup,
    ownGroups,
    peerGroups,
  )
where

import Control.Exception (throw)
import Control.Monad (forM, forM_, unless, void)
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Foreign.C.String (newCString, peekCString)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (Ptr, nullPtr)
import qualified Network.Zyre2.Bindings as ZB
import Network.Zyre2.Types
  ( ZCreated,
    ZDestroyed,
    ZRunning,
    ZStopped,
    ZyreContext (ZyreContext),
    unlessStale,
  )
import qualified Network.Zyre2.ZMsg as ZM

-- | Create a new Zyre instance/context.
-- All created contexts must be manually cleaned up with 'destroy' to avoid leaks.
-- Takes a node name, or if 'Nothing' will auto-generate a name from the node UUID.
new :: Maybe Text -> IO (ZyreContext ZCreated)
new name = do
  cname <- case name of
    Just t -> newCString (T.unpack t)
    Nothing -> pure nullPtr
  ptr <- ZB.zyreNew cname
  case name of
    Just _ -> free cname
    Nothing -> pure ()
  stale <- newIORef False
  let ctx = ZyreContext ptr stale :: ZyreContext ZCreated
  pure ctx

-- | Start the zyre instance. Starts UDP beaconing and joins the
-- peer network. Generates an 'Enter' message for other participants.
start :: ZyreContext ZCreated -> IO (ZyreContext ZRunning)
start zctx@(ZyreContext ptr stale) = unlessStale zctx $ do
  void . ZB.zyreStart $ ptr
  newStale <- newIORef False
  let ctx = ZyreContext ptr newStale :: ZyreContext ZRunning
  atomicModifyIORef' stale (const (True, ()))
  pure ctx

-- | Stop the zyre instance, leaving the peer network.
-- Generates a 'Exit' message for the other participants.
stop :: ZyreContext ZRunning -> IO (ZyreContext ZStopped)
stop zctx@(ZyreContext ptr stale) = unlessStale zctx $ do
  void . ZB.zyreStop $ ptr
  newStale <- newIORef False
  let ctx = ZyreContext ptr newStale :: ZyreContext ZStopped
  atomicModifyIORef' stale (const (True, ()))
  pure ctx

-- | Destroy the given zyre context, freeing its resources.
-- Once it has been destroyed, it can no longer be used.
-- Returns a ZyreContext tagged as destroyed to maintain an API
-- similar to the rest of the interface.
destroy :: ZyreContext ZStopped -> IO (ZyreContext ZDestroyed)
destroy zctx@(ZyreContext ptr stale) = unlessStale zctx $ do
  ZB.zyreDestroy ptr
  let ctx = ZyreContext ptr stale :: ZyreContext ZDestroyed
  atomicModifyIORef' stale (const (True, ()))
  pure ctx

-- | Join a peer group, to start receiving and be able to send
-- messages from that group. Generates a 'Join' message for
-- the other participants in the group.
join :: ZyreContext ZRunning -> Text -> IO Int
join zctx@(ZyreContext ptr _) name = unlessStale zctx $ do
  cname <- newCString (T.unpack name)
  cint <- ZB.zyreJoin ptr cname
  free cname
  pure $ fromIntegral cint

-- | Leave a peer group, and stop receiving updates from that group.
-- Generates a 'Leave' message for the other participants in the
-- group network.
leave :: ZyreContext ZRunning -> Text -> IO Int
leave zctx@(ZyreContext ptr _) name = unlessStale zctx $ do
  cname <- newCString (T.unpack name)
  cint <- ZB.zyreLeave ptr cname
  free cname
  pure $ fromIntegral cint

-- | Retrieve the UUID generated for the context.
uuid :: ZyreContext a -> IO Text
uuid zctx@(ZyreContext ptr _) = unlessStale zctx $ do
  cstring <- ZB.zyreUuid ptr
  str <- peekCString cstring
  free cstring
  pure $ T.pack str

-- | Retrieve the version of underlying zyre library.
version :: IO Word64
version = ZB.zyreVersion

-- | Retrieve the name of our node after initialization. Either set by 'new'
-- or automatically generated by zyre from the nodes UUID.
name :: ZyreContext a -> IO Text
name zctx@(ZyreContext ptr _) = unlessStale zctx $ do
  cstring <- ZB.zyreName ptr
  str <- peekCString cstring
  free cstring
  pure $ T.pack str

-- | Shout a 'Shout' to a group. Sends data frames.
--
-- > ctx <- new "my-node"
-- > ctx <- start ctx
-- > join ctx "my-group"
-- > let msg = addString "My message" msgShout
-- > shout ctx "my-group" msg
--
-- You can also send multiple frames in the same message.
--
-- > let msg = addString "Frame2" . addString "Frame1" $ msgShout
shout :: ZyreContext ZRunning -> Text -> ZM.ZMsg -> IO Int
shout zctx@(ZyreContext ptr _) room zmsg@ZM.Shout {} = unlessStale zctx $ do
  msg_ptr <- ZB.zyreNewZMsg
  croom <- newCString (T.unpack room)
  forM_ (ZM._zmsgMessage zmsg) $ \frame -> ZB.zyreAddFrame msg_ptr (ZM.frameData frame)
  cint <- ZB.zyreShout ptr croom msg_ptr
  free croom
  pure $ fromIntegral cint
shout _ _ _ = pure (-1)

-- | Shout a 'Shout' to a group. Sends some 'Text' value encoded as a 'Data.ByteString.ByteString'.
--
-- > ctx <- new "my-node"
-- > ctx <- start ctx
-- > join ctx "my-group"
-- > shout ctx "my-group" "My message"
shouts :: ZyreContext ZRunning -> Text -> Text -> IO Int
shouts zctx@(ZyreContext ptr _) room msg = unlessStale zctx $ do
  croom <- newCString (T.unpack room)
  cmsg <- newCString (T.unpack msg)
  cint <- ZB.zyreShouts ptr croom cmsg
  free croom
  free cmsg
  pure $ fromIntegral cint

-- | Whisper a 'Whisper' to a specific peer. Sends data frames
--
-- > ctx <- new "my-node"
-- > ctx <- start ctx
-- > let msg = addString "My message" msgWhisper
-- > whisper ctx "other-node" msg
whisper :: ZyreContext ZRunning -> Text -> ZM.ZMsg -> IO Int
whisper zctx@(ZyreContext ptr _) peer zmsg@ZM.Whisper {} = unlessStale zctx $ do
  msg_ptr <- ZB.zyreNewZMsg
  cpeer <- newCString (T.unpack peer)
  forM_ (ZM._zmsgMessage zmsg) $ \frame -> ZB.zyreAddFrame msg_ptr (ZM.frameData frame)
  cint <- ZB.zyreWhisper ptr cpeer msg_ptr
  free cpeer
  pure $ fromIntegral cint
whisper _ _ _ = pure (-1)

-- | Whisper a 'Whisper' to a specific peer. Sends some 'Text' value encoded as a 'Data.ByteString.ByteString'.
--
-- > ctx <- new "my-node"
-- > ctx <- start ctx
-- > let msg = addString "My message" msgWhisper
-- > whisper ctx "other-node" msg
whispers :: ZyreContext ZRunning -> Text -> Text -> IO Int
whispers zctx@(ZyreContext ptr _) peer msg = unlessStale zctx $ do
  cpeer <- newCString (T.unpack peer)
  cmsg <- newCString (T.unpack msg)
  cint <- ZB.zyreWhispers ptr cpeer cmsg
  free cpeer
  free cmsg
  pure $ fromIntegral cint

-- | Block and await a message from the peer network.
-- Returns 'Nothing' if it times out or is interruped.
recv :: ZyreContext ZRunning -> IO (Maybe ZM.ZMsg)
recv zctx@(ZyreContext ptr _) = unlessStale zctx $ do
  -- Block and listen for a msg, receive a pointer to msg or null.
  msg_ptr <- ZB.zyreRecv ptr

  returnValRef <- newIORef Nothing
  unless (msg_ptr == nullPtr) $ do
    cmsgType <- ZB.zyrePopStrFrame msg_ptr
    msgType <- peekCString cmsgType

    free cmsgType

    case msgType of
      "ENTER" -> do
        -- Pop the message metadata off the zmsg, marshal into haskell types.
        cfromnode <- ZB.zyrePopStrFrame msg_ptr
        cname <- ZB.zyrePopStrFrame msg_ptr
        cheader_ptr <- ZB.zyrePopFrame msg_ptr
        cipport <- ZB.zyrePopStrFrame msg_ptr
        fromnode <- peekCString cfromnode
        name <- peekCString cname
        ipport <- peekCString cipport

        -- Unpack the headers
        headersRef <- newIORef []
        headers_ptr <- ZB.zyreUnpackHeaders cheader_ptr
        extractHeaders headers_ptr headersRef
        ZB.zyreDestroyHeaders headers_ptr
        headers <- readIORef headersRef

        -- Release the allocated resources
        ZB.zyreDestroyFrame cheader_ptr
        free cipport
        free cname
        free cfromnode

        -- Construct the message
        let msg =
              ZM.Enter
                { ZM._zmsgFromNode = T.pack fromnode,
                  ZM._zmsgName = T.pack name,
                  ZM._zmsgHeaders = headers,
                  ZM._zmsgIpPort = T.pack ipport
                }
        -- Write our resulting message into the message buffer.
        atomicModifyIORef' returnValRef (const (Just msg, ()))
      "EVASIVE" -> do
        cfromnode <- ZB.zyrePopStrFrame msg_ptr
        cname <- ZB.zyrePopStrFrame msg_ptr
        fromnode <- peekCString cfromnode
        name <- peekCString cname

        free cfromnode
        free cname

        let msg =
              ZM.Evasive
                { ZM._zmsgFromNode = T.pack fromnode,
                  ZM._zmsgName = T.pack name
                }
        atomicModifyIORef' returnValRef (const (Just msg, ()))
      "SILENT" -> do
        cfromnode <- ZB.zyrePopStrFrame msg_ptr
        cname <- ZB.zyrePopStrFrame msg_ptr
        fromnode <- peekCString cfromnode
        name <- peekCString cname

        free cfromnode
        free cname

        let msg =
              ZM.Silent
                { ZM._zmsgFromNode = T.pack fromnode,
                  ZM._zmsgName = T.pack name
                }
        atomicModifyIORef' returnValRef (const (Just msg, ()))
      "EXIT" -> do
        cfromnode <- ZB.zyrePopStrFrame msg_ptr
        cname <- ZB.zyrePopStrFrame msg_ptr
        fromnode <- peekCString cfromnode
        name <- peekCString cname

        free cfromnode
        free cname

        let msg =
              ZM.Exit
                { ZM._zmsgFromNode = T.pack fromnode,
                  ZM._zmsgName = T.pack name
                }
        atomicModifyIORef' returnValRef (const (Just msg, ()))
      "JOIN" -> do
        cfromnode <- ZB.zyrePopStrFrame msg_ptr
        cname <- ZB.zyrePopStrFrame msg_ptr
        cgroupname <- ZB.zyrePopStrFrame msg_ptr
        fromnode <- peekCString cfromnode
        name <- peekCString cname
        groupname <- peekCString cgroupname

        free cfromnode
        free cname
        free cgroupname

        let msg =
              ZM.Join
                { ZM._zmsgFromNode = T.pack fromnode,
                  ZM._zmsgName = T.pack name,
                  ZM._zmsgGroupName = T.pack groupname
                }
        atomicModifyIORef' returnValRef (const (Just msg, ()))
      "LEAVE" -> do
        cfromnode <- ZB.zyrePopStrFrame msg_ptr
        cname <- ZB.zyrePopStrFrame msg_ptr
        cgroupname <- ZB.zyrePopStrFrame msg_ptr
        fromnode <- peekCString cfromnode
        name <- peekCString cname
        groupname <- peekCString cgroupname

        free cfromnode
        free cname
        free cgroupname

        let msg =
              ZM.Leave
                { ZM._zmsgFromNode = T.pack fromnode,
                  ZM._zmsgName = T.pack name,
                  ZM._zmsgGroupName = T.pack groupname
                }
        atomicModifyIORef' returnValRef (const (Just msg, ()))
      "WHISPER" -> do
        cfromnode <- ZB.zyrePopStrFrame msg_ptr
        cname <- ZB.zyrePopStrFrame msg_ptr
        fromnode <- peekCString cfromnode
        name <- peekCString cname

        free cfromnode
        free cname

        msgBodyRef <- newIORef []
        extractFrames msg_ptr msgBodyRef
        msgBody <- readIORef msgBodyRef

        let msg =
              ZM.Whisper
                { ZM._zmsgFromNode = T.pack fromnode,
                  ZM._zmsgName = T.pack name,
                  ZM._zmsgMessage = msgBody
                }

        atomicModifyIORef' returnValRef (const (Just msg, ()))
      "SHOUT" -> do
        cfromnode <- ZB.zyrePopStrFrame msg_ptr
        cname <- ZB.zyrePopStrFrame msg_ptr
        cgroupname <- ZB.zyrePopStrFrame msg_ptr
        fromnode <- peekCString cfromnode
        name <- peekCString cname
        groupname <- peekCString cgroupname

        free cfromnode
        free cname
        free cgroupname

        msgBodyRef <- newIORef []
        extractFrames msg_ptr msgBodyRef
        msgBody <- readIORef msgBodyRef

        let msg =
              ZM.Shout
                { ZM._zmsgFromNode = T.pack fromnode,
                  ZM._zmsgName = T.pack name,
                  ZM._zmsgGroupName = T.pack groupname,
                  ZM._zmsgMessage = msgBody
                }

        atomicModifyIORef' returnValRef (const (Just msg, ()))

      -- There is an undocumented 'STOP' message released when the zyre
      -- context is stopped.
      "STOP" -> atomicModifyIORef' returnValRef (const (Just ZM.Stop, ()))
      _ -> putStrLn $ "Unhandled msgType: " <> msgType

  returnVal <- readIORef returnValRef
  ZB.zyreZmsgDestroy msg_ptr
  pure returnVal

-- | List the id of the peers in the peer network.
peers :: ZyreContext ZRunning -> IO [Text]
peers zctx@(ZyreContext ptr _) = unlessStale zctx $ do
  list_ptr <- ZB.zyrePeers ptr
  returnRef <- newIORef []
  extractList list_ptr returnRef
  ZB.zyreZListDestroy list_ptr
  readIORef returnRef

-- | List the id of the peers in a specific group in the peer network.
peersByGroup :: ZyreContext ZRunning -> Text -> IO (Maybe [Text])
peersByGroup zctx@(ZyreContext ptr _) group = unlessStale zctx $ do
  returnRef <- newIORef []
  cgroup <- newCString (T.unpack group)
  list_ptr <- ZB.zyrePeersByGroup ptr cgroup
  if list_ptr /= nullPtr
    then do
      extractList list_ptr returnRef
      ZB.zyreZListDestroy list_ptr
    else pure ()
  free cgroup
  (\xs -> if null xs then Nothing else Just xs) <$> readIORef returnRef

-- | Retrieve the endpoint of a connected peer.
-- Returns 'Nothing' if peer does not exist.
peerAddress :: ZyreContext ZRunning -> Text -> IO (Maybe Text)
peerAddress zctx@(ZyreContext ptr _) peer = unlessStale zctx $ do
  cpeer <- newCString (T.unpack peer)
  caddress <- ZB.zyrePeerAddress ptr cpeer
  address <- T.pack <$> peekCString caddress
  free cpeer
  free caddress
  pure $ if T.null address then Nothing else Just address

-- | Retrieve the value of a header of a connected peer.
-- Returns 'Nothing' if peer or key doesn't exist.
peerHeaderValue :: ZyreContext ZRunning -> Text -> Text -> IO (Maybe Text)
peerHeaderValue zctx@(ZyreContext ptr _) peer header = unlessStale zctx $ do
  cpeer <- newCString (T.unpack peer)
  cheader <- newCString (T.unpack header)
  cvalue <- ZB.zyrePeerHeaderValue ptr cpeer cheader
  value <- if cvalue == nullPtr then pure T.empty else T.pack <$> peekCString cvalue
  free cpeer
  free cheader
  free cvalue
  pure $ if T.null value then Nothing else Just value

-- | List the groups that you are a part of.
ownGroups :: ZyreContext ZRunning -> IO [Text]
ownGroups zctx@(ZyreContext ptr _) = unlessStale zctx $ do
  list_ptr <- ZB.zyreOwnGroups ptr
  returnRef <- newIORef []
  extractList list_ptr returnRef
  ZB.zyreZListDestroy list_ptr
  readIORef returnRef

-- | List groups that are known through connected peers.
peerGroups :: ZyreContext ZRunning -> IO [Text]
peerGroups zctx@(ZyreContext ptr _) = unlessStale zctx $ do
  list_ptr <- ZB.zyreOwnGroups ptr
  returnRef <- newIORef []
  extractList list_ptr returnRef
  ZB.zyreZListDestroy list_ptr
  readIORef returnRef

-- | Internal helper function.
-- Traverses a zmsg using next() and accumulates the frames in an IORef.
extractFrames :: Ptr () -> IORef [ZM.ZFrame] -> IO ()
extractFrames msg_ptr framesRef = do
  cursor <- ZB.zyreNextFrame msg_ptr
  if cursor /= nullPtr
    then do
      len <- ZB.zyreFrameSize cursor
      content_ptr <- ZB.zyreFrameData cursor
      packed <- BS.packCStringLen (content_ptr, fromIntegral len)
      atomicModifyIORef' framesRef (\x -> (x <> [ZM.mkFrame packed], ()))
      extractFrames msg_ptr framesRef
    else pure ()

-- | Internal helper function.
-- Traverses an unpacked zhash table, using next() and cursor() to
-- accumulate the stored values into a dictionary in an IORef.
extractHeaders :: Ptr () -> IORef [(Text, Text)] -> IO ()
extractHeaders header_ptr headersRef = do
  cursor <- ZB.zyreNextHeader header_ptr
  if cursor /= nullPtr
    then do
      key_ptr <- ZB.zyreHeaderCursor header_ptr
      key <- peekCString key_ptr
      val <- peekCString cursor

      atomicModifyIORef' headersRef (\x -> (x <> [(T.pack key, T.pack val)], ()))
      extractHeaders header_ptr headersRef
    else pure ()

-- | Internal helper function.
-- Extracts values in a zlist as text values into an IORef.
extractList :: Ptr () -> IORef [Text] -> IO ()
extractList list_ptr accumRef = do
  cursor <- ZB.zyreZListNext list_ptr
  if cursor /= nullPtr
    then do
      val <- peekCString cursor
      atomicModifyIORef' accumRef (\x -> (x <> [T.pack val], ()))
      extractList list_ptr accumRef
    else pure ()