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
-- This package provides a haskell interface to the Zyre 2.0.1 API. The
-- package requires the c libraries czmq and zyre to be installed on the
-- system. See https://github.com/zeromq/zyre for specifics.
module Network.Zyre2
  ( -- * Context Lifecycle
    ZyreContext,
    new,
    start,
    stop,
    destroy,

    -- * Context information
    uuid,
    name,
    version,

    -- * Group membership
    join,
    leave,
    ownGroups,
    peerGroups,

    -- * Peers
    peers,
    peersByGroup,
    peerAddress,
    peerHeaderValue,

    -- * Sending and receiving messages
    shout,
    shouts,
    whisper,
    whispers,
    recv,

    -- * Constructing messages
    ZMsg (..),
    msgShout,
    msgWhisper,

    -- ** Adding data
    pushMem,
    pushText,
    addMem,
    addText,

    -- ** Frames
    ZFrame,
    frameData,

    -- ** Deconstructing messages
    popText,
    popMem,

    -- * Phantom tags for lifecycle state
    ZCreated,
    ZDestroyed,
    ZRunning,
    ZStopped,
  )
where

import Network.Zyre2.Types
  ( ZCreated,
    ZDestroyed,
    ZRunning,
    ZStopped,
    ZyreContext,
  )
import Network.Zyre2.ZMsg
  ( ZFrame,
    ZMsg (..),
    addMem,
    addText,
    frameData,
    msgShout,
    msgWhisper,
    popMem,
    popText,
    pushMem,
    pushText,
  )
import Network.Zyre2.Zyre
  ( destroy,
    join,
    leave,
    name,
    new,
    ownGroups,
    peerGroups,
    peers,
    peerAddress,
    peerHeaderValue,
    peersByGroup,
    recv,
    shout,
    shouts,
    start,
    stop,
    uuid,
    version,
    whisper,
    whispers,
  )
