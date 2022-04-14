{-# LANGUAGE OverloadedStrings #-}

-- | Message types for sending and receiving from a Zyre peer network.
module Network.Zyre2.ZMsg
  ( ZMsg (..),
    ZFrame,
    pop,
    pushText,
    addText,
    popText,
    pushMem,
    addMem,
    popMem,
    mkFrame,
    frameData,
    msgWhisper,
    msgShout,
  )
where

import Control.Exception (throw)
import qualified Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Zyre2.Types (ZyreException (ZyreMsgDontSupportFramesException))

-- | Message-types supported and published by zyre.
data ZMsg
  = -- | A peer has joined the peer network.
    Enter
      { -- | The UUID of the sending node.
        _zmsgFromNode :: Text,
        -- | The name of the sending node.
        _zmsgName :: Text,
        -- | Dictionary containing the headers.
        _zmsgHeaders :: [(Text, Text)],
        -- | The ip and port of the sending node. E.g. tcp://127.0.0.1:8344
        _zmsgIpPort :: Text
      }
  | -- | A peer has not sent a message within the 'evasive' interval period.
    -- The peer will be pinged.
    Evasive
      { _zmsgFromNode :: Text,
        _zmsgName :: Text
      }
  | -- | A peer has been silent and not responded to PING messages for the 'silent' interval period.
    Silent
      { _zmsgFromNode :: Text,
        _zmsgName :: Text
      }
  | -- | A peer has exited the peer network.
    Exit
      { _zmsgFromNode :: Text,
        _zmsgName :: Text
      }
  | -- | A peer has joined a group.
    Join
      { _zmsgFromNode :: Text,
        _zmsgName :: Text,
        _zmsgGroupName :: Text
      }
  | -- | A peer has left a group.
    Leave
      { _zmsgFromNode :: Text,
        _zmsgName :: Text,
        -- | The name of the group which the message concerns.
        _zmsgGroupName :: Text
      }
  | -- | A peer has whispered to this node.
    Whisper
      { _zmsgFromNode :: Text,
        _zmsgName :: Text,
        -- | The message content, coded as 'ZFrame's. A message may hold 0 or more frames.
        -- Note: a ZMsg with 0 frames supplied to 'Network.Zyre2.shout' or 'Network.Zyre2.whisper' will be ignored and not sent.
        _zmsgMessage :: [ZFrame]
      }
  | -- | A peer has shouted in a group.
    Shout
      { _zmsgFromNode :: Text,
        _zmsgName :: Text,
        _zmsgGroupName :: Text,
        _zmsgMessage :: [ZFrame]
      }
  | -- | The zyre context which is being listened to has been stopped.
    Stop
  deriving (Show)

-- | A frame of binary data. See 'Network.Zyre2.ZMsg.mkFrame' and 'Network.Zyre2.ZMsg.frameData'.
newtype ZFrame = ZFrame ByteString

instance Show ZFrame where
  show (ZFrame _) = "ZFrame[Bytes]"

{- Utility functions -}

pushFrame :: ZMsg -> ZFrame -> ZMsg
pushFrame zmsg@Whisper {} zframe = zmsg {_zmsgMessage = zframe : _zmsgMessage zmsg}
pushFrame zmsg@Shout {} zframe = zmsg {_zmsgMessage = zframe : _zmsgMessage zmsg}
pushFrame _ _ = throw ZyreMsgDontSupportFramesException

addFrame :: ZMsg -> ZFrame -> ZMsg
addFrame zmsg@Whisper {} zframe = zmsg {_zmsgMessage = _zmsgMessage zmsg <> [zframe]}
addFrame zmsg@Shout {} zframe = zmsg {_zmsgMessage = _zmsgMessage zmsg <> [zframe]}
addFrame _ _ = throw ZyreMsgDontSupportFramesException

-- {- Interface -}

append :: ZMsg -> ZFrame -> ZMsg
append = addFrame

prepend :: ZMsg -> ZFrame -> ZMsg
prepend = pushFrame

-- | Remove the first frame of a ZMsg, or return 'Nothing'.
pop :: ZMsg -> (Maybe ZFrame, ZMsg)
pop zmsg@Whisper {} = _popMsg zmsg
pop zmsg@Shout {} = _popMsg zmsg
pop _ = throw ZyreMsgDontSupportFramesException

_popMsg :: ZMsg -> (Maybe ZFrame, ZMsg)
_popMsg m = case _zmsgMessage m of
  [] -> (Nothing, m)
  [x] -> (Just x, m {_zmsgMessage = []})
  (x : _) -> (Just x, m {_zmsgMessage = List.tail (_zmsgMessage m)})

-- | Push a text frame to the front of a 'ZMsg'.
pushText :: Text -> ZMsg -> ZMsg
pushText str zmsg = zmsg `pushFrame` ZFrame (fromString (T.unpack str))

-- | Add a text frame to the end of a 'ZMsg'.
addText :: Text -> ZMsg -> ZMsg
addText str zmsg = zmsg `addFrame` ZFrame (fromString (T.unpack str))

-- | Push binary data to the front of a 'ZMsg' as a frame.
pushMem :: ByteString -> ZMsg -> ZMsg
pushMem bs zmsg = zmsg `pushFrame` ZFrame bs

-- | Add binary data to the end of a 'ZMsg' as a frame.
addMem :: ByteString -> ZMsg -> ZMsg
addMem bs zmsg = zmsg `addFrame` ZFrame bs

-- | Remove the first frame of the 'ZMsg', and interpret it as 'Text'.
popText :: ZMsg -> (Maybe Text, ZMsg)
popText zmsg =
  Data.Bifunctor.first
    (fmap (\(ZFrame bs) -> T.pack (BSC.unpack bs)))
    (pop zmsg)

-- | Remove the first frame of the 'ZMsg' as a 'ByteString'.
popMem :: ZMsg -> (Maybe ByteString, ZMsg)
popMem zmsg =
  Data.Bifunctor.first
    (fmap (\(ZFrame bs) -> bs))
    (pop zmsg)

-- | Create a frame from a ByteString.
mkFrame :: ByteString -> ZFrame
mkFrame = ZFrame

-- | Retrieve the data stored in a 'ZFrame'.
frameData :: ZFrame -> ByteString
frameData (ZFrame bs) = bs

-- | Empty 'Whisper' message. Combine with 'addText', 'pushText',
-- 'addMem', and 'pushMem' to construct a whisper to send.
msgWhisper :: ZMsg
msgWhisper =
  Whisper
    { _zmsgFromNode = "Not set",
      _zmsgName = "Not set",
      _zmsgMessage = []
    }

-- | Empty 'Shout' message. Combine with 'addText', 'pushText',
-- 'addMem', and 'pushMem' to construct a shout to send.
msgShout :: ZMsg
msgShout =
  Shout
    { _zmsgFromNode = "Not set",
      _zmsgName = "Not set",
      _zmsgGroupName = "Not set",
      _zmsgMessage = []
    }