{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides minimal wrapping between C and Haskell.
--
-- Prefer to use 'Network.Zyre2.Zyre2' instead, or you'll need intimate knowledge
-- of the C library requirements to avoid segfaults, manually free memory
-- and similar.
--
-- This module deals entirely with C-typed data from Haskells FFI.
module Network.Zyre2.Bindings where

import Data.ByteString (ByteString)
import Foreign (Ptr)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt)
import qualified Language.C.Inline as C
import Data.Word (Word64)

C.context (C.baseCtx <> C.bsCtx)
C.include "zyre.h"

-- Lifecycle management bindings

zyreNew :: CString -> IO (Ptr ())
zyreNew name = do
  [C.exp| void* { zyre_new($(char* name)) } |]

zyreStart :: Ptr () -> IO CInt
zyreStart ptr =
  [C.exp| int { zyre_start($(void* ptr)) } |]

zyreStop :: Ptr () -> IO ()
zyreStop ptr = do
  [C.exp| void { zyre_stop($(void* ptr)) } |]

zyreDestroy :: Ptr () -> IO ()
zyreDestroy ptr = do
  [C.block| void {
      zyre_t* node = $(void* ptr);
      zyre_destroy(&node);
  } |]

-- Info

zyreUuid :: Ptr () -> IO CString
zyreUuid ptr = do
  [C.exp| const char* { zyre_uuid($(void* ptr)) } |]

zyreName :: Ptr () -> IO CString
zyreName ptr = do
  [C.exp| const char* { zyre_name($(void* ptr)) } |]

zyreVersion :: IO Word64
zyreVersion = do
  [C.exp| uint64_t { zyre_version() } |]

-- Configuration

zyreSetName :: Ptr () -> CString -> IO ()
zyreSetName ptr name = do
  [C.exp| void { zyre_set_name($(void* ptr), $(const char* name)) } |]

zyreSetHeader :: Ptr () -> CString -> CString -> IO ()
zyreSetHeader ptr header_name header_value = do
  [C.exp| void { zyre_set_header($(void* ptr), $(const char* header_name), "%s", $(const char* header_value)) } |]

zyreSetVerbose :: Ptr () -> IO ()
zyreSetVerbose ptr = do
  [C.exp| void { zyre_set_verbose($(void* ptr)) } |]

zyreSetPort :: Ptr () -> CInt -> IO ()
zyreSetPort ptr port = do
  [C.exp| void { zyre_set_port($(void* ptr), $(int port)) } |]

zyreSetEvasiveTimeout :: Ptr () -> CInt -> IO ()
zyreSetEvasiveTimeout ptr timeout = do
  [C.exp| void { zyre_set_evasive_timeout($(void* ptr), $(int timeout)) } |]

zyreSetSilentTimeout :: Ptr () -> CInt -> IO ()
zyreSetSilentTimeout ptr timeout = do
  [C.exp| void { zyre_set_silent_timeout($(void* ptr), $(int timeout)) } |]

zyreSetExpiredTimeout :: Ptr () -> CInt -> IO ()
zyreSetExpiredTimeout ptr timeout = do
  [C.exp| void { zyre_set_expired_timeout($(void* ptr), $(int timeout)) } |]

zyreSetInterval :: Ptr () -> CInt -> IO ()
zyreSetInterval ptr interval = do
  [C.exp| void { zyre_set_interval($(void* ptr), $(int interval)) } |]

zyreSetInterface :: Ptr () -> CString -> IO ()
zyreSetInterface ptr interface = do
  [C.exp| void { zyre_set_interface($(void* ptr), $(const char* interface)) } |]

-- Zyre 'basic' use

zyreJoin :: Ptr () -> CString -> IO CInt
zyreJoin ptr room = do
  [C.exp| int { zyre_join($(void* ptr), $(char* room)) } |]

zyreLeave :: Ptr () -> CString -> IO CInt
zyreLeave ptr room = do
  [C.exp| int { zyre_leave($(void* ptr), $(char* room)) } |]

zyreShout :: Ptr () -> CString -> Ptr () -> IO CInt
zyreShout ctx_ptr room msg_ptr = do
  [C.block| int {
      zmsg_t* msg = $(void* msg_ptr); 
      return zyre_shout($(void* ctx_ptr), $(const char* room), &msg);
  } |]

zyreShouts :: Ptr () -> CString -> CString -> IO CInt
zyreShouts ptr room msg = do
  [C.exp| int { zyre_shouts($(void* ptr), $(const char* room), "%s", $(const char* msg)) } |]

zyreWhisper :: Ptr () -> CString -> Ptr () -> IO CInt
zyreWhisper ctx_ptr peer msg_ptr = do
  [C.block| int {
      zmsg_t* msg = $(void* msg_ptr); 
      return zyre_whisper($(void* ctx_ptr), $(const char* peer), &msg);
  } |]

zyreWhispers :: Ptr () -> CString -> CString -> IO CInt
zyreWhispers ptr peer msg = do
  [C.exp| int { zyre_whispers($(void* ptr), $(const char* peer), "%s", $(const char* msg)) } |]

zyreRecv :: Ptr () -> IO (Ptr ())
zyreRecv ptr = do
  [C.exp| void* { zyre_recv($(void* ptr)) } |]

zyrePopStrFrame :: Ptr () -> IO CString
zyrePopStrFrame ptr = do
  [C.exp| char* { zmsg_popstr($(void* ptr)) } |]

zyrePopFrame :: Ptr () -> IO (Ptr ())
zyrePopFrame ptr = do
  [C.exp| void* { zmsg_pop($(void* ptr)) } |]

zyreNextFrame :: Ptr () -> IO (Ptr ())
zyreNextFrame ptr = do
  [C.exp| void* { zmsg_next($(void* ptr)) } |]

zyreFrameSize :: Ptr () -> IO CInt
zyreFrameSize ptr = do
  [C.exp| int { zframe_size($(void* ptr)) } |]

zyreFrameData :: Ptr () -> IO (Ptr C.CChar)
zyreFrameData ptr = do
  [C.exp| char* { zframe_data($(void* ptr)) } |]

zyreDestroyFrame :: Ptr () -> IO ()
zyreDestroyFrame ptr = do
  [C.block| void {
    zframe_t* frame = $(void* ptr);
    zframe_destroy(&frame);
  } |]

zyreNewZMsg :: IO (Ptr ())
zyreNewZMsg = do
  [C.exp| void* { zmsg_new() } |]

zyreAddFrame :: Ptr () -> ByteString -> IO CInt
zyreAddFrame ptr bs = do
  [C.exp| int { zmsg_addmem($(void* ptr), $bs-ptr:bs, $bs-len:bs) } |]

{- Functions regarding zmsg_t -}

zyreZmsgPrint :: Ptr () -> IO ()
zyreZmsgPrint ptr = do
  [C.exp| void { zmsg_print($(void* ptr))} |]

zyreZmsgDestroy :: Ptr () -> IO ()
zyreZmsgDestroy ptr = do
  [C.block| void {
      zmsg_t *msg = $(void* ptr); 
      zmsg_destroy(&msg);
  } |]

{- Functions regarding headers -}

zyreUnpackHeaders :: Ptr () -> IO (Ptr ())
zyreUnpackHeaders ptr =
  [C.exp| void* { zhash_unpack($(void* ptr)) } |]

zyreNextHeader :: Ptr () -> IO CString
zyreNextHeader ptr =
  [C.exp| const char* { zhash_next($(void* ptr)) } |]

zyreHeaderCursor :: Ptr () -> IO CString
zyreHeaderCursor ptr =
  [C.exp| const char* { zhash_cursor($(void* ptr)) } |]

zyreDestroyHeaders :: Ptr () -> IO ()
zyreDestroyHeaders ptr =
  [C.block| void {
    zhash_t* table = $(void* ptr); 
    zhash_destroy(&table);
  } |]

{- Functions regarding peer lookup -}

zyrePeers :: Ptr () -> IO (Ptr ())
zyrePeers ptr =
  [C.exp| void* { zyre_peers($(void* ptr))} |] 

zyrePeersByGroup :: Ptr () -> CString -> IO (Ptr ())
zyrePeersByGroup ptr str =
  [C.exp| void* { zyre_peers_by_group($(void* ptr), $(const char* str))} |]

zyrePeerAddress :: Ptr () -> CString -> IO CString
zyrePeerAddress ptr str =
  [C.exp| const char* { zyre_peer_address($(void* ptr), $(const char* str))} |]

zyrePeerHeaderValue :: Ptr () -> CString -> CString -> IO CString
zyrePeerHeaderValue ptr peer headerValue =
  [C.exp| const char* { zyre_peer_header_value($(void* ptr), $(const char* peer), $(const char* headerValue))} |]

{- Lookups regarding groups -}

zyreOwnGroups :: Ptr () -> IO (Ptr ())
zyreOwnGroups ptr =
  [C.exp| void* { zyre_own_groups($(void* ptr))} |] 

zyrePeerGroups :: Ptr () -> IO (Ptr ())
zyrePeerGroups ptr =
  [C.exp| void* { zyre_peer_groups($(void* ptr))} |] 



{- Functions regarding zlist_t -}

zyreZListNext :: Ptr () -> IO CString
zyreZListNext ptr =
  [C.exp| const char* { zlist_next($(void* ptr)) } |] 

zyreZListDestroy :: Ptr () -> IO ()
zyreZListDestroy ptr =
  [C.block| void {
    zlist_t* list = $(void* ptr);
    zlist_destroy(&list);
  } |]