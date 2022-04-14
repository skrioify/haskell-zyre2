{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, join, void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text as T (Text, pack, unpack)
import qualified Network.Zyre2 as Zyre
import System.Environment (getArgs)

-- | Main function that handles example selection.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ("minimal" : x : _) -> minimal (T.pack x)
    ("chat" : x : _) -> chat (T.pack x)
    _ -> error "Unknown example."

-- | Zyre chat example adapted from examples/minimal/minimal.c
minimal :: Text -> IO ()
minimal name = do
  let room = "ROOM"

  ctx <- Zyre.new (Just name)
  ctx <- Zyre.start ctx
  Zyre.join ctx room

  forM_ [0 .. 10] $ \_ -> do
    Zyre.shouts ctx room ("Hello from " <> name)

    threadDelay $ 1000 * 250
    mMsg <- Zyre.recv ctx
    print mMsg
    case mMsg of
      Just Zyre.Shout {} -> print (fst . Zyre.popText =<< mMsg)
      _ -> pure ()

  Zyre.leave ctx room

  ctx <- Zyre.stop ctx
  void . Zyre.destroy $ ctx

-- | Zyre chat example adapted from examples/chat/chat.c
chat :: Text -> IO ()
chat name = do
  let room = "CHAT"

  terminateRef <- newIORef False

  ctx <- Zyre.new (Just name)
  ctx <- Zyre.start ctx
  Zyre.join ctx room

  -- Fork a thread to receive messages and output them to stdout.
  void . forkIO $
    whileM_ (readIORef terminateRef) $ do
      mMsg <- Zyre.recv ctx
      case mMsg of
        Just zmsg@Zyre.Shout {} -> do
          putStr $ T.unpack $ Zyre._zmsgName zmsg <> ": "
          case Zyre.popText zmsg of
            (Just m, _) -> putStrLn $ T.unpack m
            _ -> putStrLn "Message could not be decoded or was empty"
        Just zmsg@Zyre.Join {} -> putStrLn $ T.unpack $ Zyre._zmsgName zmsg <> " has joined the chat"
        Just zmsg@Zyre.Leave {} -> putStrLn $ T.unpack $ Zyre._zmsgName zmsg <> " has left the chat"
        Just zmsg@Zyre.Evasive {} -> putStrLn $ T.unpack $ Zyre._zmsgName zmsg <> " is being evasive"
        Just zmsg@Zyre.Silent {} -> putStrLn $ T.unpack $ Zyre._zmsgName zmsg <> " is being silent"
        _ -> pure ()

  whileM_ (readIORef terminateRef) $ do
    line <- fmap T.pack getLine
    case line of
      "$TERM" -> do
        writeIORef terminateRef True
        Zyre.leave ctx room
        ctx <- Zyre.stop ctx
        void $ Zyre.destroy ctx
      _ -> void $ Zyre.shouts ctx room line

-- | Loop over an action until monadic condition is fulfilled to stop.
whileM_ :: (Monad m) => m Bool -> m () -> m ()
whileM_ condition fn = do
  stop <- condition
  if not stop
    then do
      fn
      whileM_ condition fn
    else pure ()