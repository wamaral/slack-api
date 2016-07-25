{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- | The handle-based API. You may find the monadic API (found in
-- "Web.Slack.Monad") more convenient.
--
-- > main :: IO ()
-- > main = withSlackHandle myConfig echobot
-- >
-- > echobot :: SlackHandle -> IO ()
-- > echobot h = forever $ getNextEvent h >>= \case
-- >     Message cid _ msg _ _ _ -> sendMessage cid msg h
-- >     _ -> return ()
--
-- This style is typically more verbose than code written with the 'Slack'
-- monad, but it is more flexible. For instance, consider forking multiple
-- threads which share a single handle.
--
module Web.Slack.Handle
    ( SlackHandle
    , withSlackHandle

      -- * Handle-based API
    , getNextEvent
    , getSession
    , getConfig
    , sendPing
    , sendMessage
    , sendRichMessage

      -- * Types
    , SlackConfig(..)
    , SlackSession(..)
    , Event(..)
    , module Web.Slack.Types
    ) where

import Control.Error
import Control.Monad.Except
import Data.Aeson
import Data.IORef
import Data.Monoid
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import qualified Network.URI as URI
import qualified Network.WebSockets as WS
import Web.Slack.Types
import Web.Slack.WebAPI
import Wuss

data SlackHandle = SlackHandle
    { _shConfig     :: SlackConfig
    , _shSession    :: SlackSession
    , _shConnection :: WS.Connection
    , _shCounter    :: IORef Int
    }

withSlackHandle :: SlackConfig -> (SlackHandle -> IO a) -> IO a
withSlackHandle conf fn = crashOnError $ do
    (url, sessionInfo) <- rtm_start conf
    (host, path) <- parseWebSocketUrl url
    liftIO $ runSecureClient host 443 path $ \conn -> do
        freshCounter <- newIORef 0
        let h = SlackHandle
              { _shConfig = conf
              , _shConnection = conn
              , _shSession = sessionInfo
              , _shCounter = freshCounter
              }
        WS.forkPingThread conn 10
        fn h

parseWebSocketUrl :: Monad m => T.Text -> ExceptT T.Text m (String, String)
parseWebSocketUrl url = do
    uri  <- URI.parseURI (T.unpack url) ?? ("Couldn't parse WebSockets URL: " <> url)
    auth <- URI.uriAuthority uri ?? ("No authority: " <> url)
    return (URI.uriRegName auth, URI.uriPath uri)

getConfig :: SlackHandle -> SlackConfig
getConfig = _shConfig

getSession :: SlackHandle -> SlackSession
getSession = _shSession

getNextEvent :: SlackHandle -> IO Event
getNextEvent h@SlackHandle{..} = do
    raw <- WS.receiveData _shConnection
    case eitherDecode raw of
        Left e -> do
            putStrLn $ unlines
                [ show raw
                , e
                , "Please report this failure to the github issue tracker"
                ]
            getNextEvent h
        Right event@(UnknownEvent val) -> do
            putStrLn $ unlines
                [ show val
                , "Failed to parse to a known event"
                , "Please report this failure to the github issue tracker"
                ]
            return event
        Right event ->
            return event

nextMessageId :: SlackHandle -> IO Int
nextMessageId SlackHandle{..} = do
    liftIO $ modifyIORef _shCounter (+1)
    liftIO $ readIORef _shCounter

sendMessage :: SlackHandle -> ChannelId -> T.Text -> IO ()
sendMessage h@SlackHandle{..} cid message = do
    uid <- nextMessageId h
    let payload = MessagePayload uid "message" cid message
    WS.sendTextData _shConnection (encode payload)

sendPing :: SlackHandle -> IO ()
sendPing h@SlackHandle{..} = do
    uid <- nextMessageId h
    now <- round <$> getPOSIXTime
    let payload = PingPayload uid "ping" now
    WS.sendTextData _shConnection (encode payload)

sendRichMessage
    :: SlackHandle -> ChannelId -> T.Text -> [Attachment] -> IO (Either T.Text ())
sendRichMessage h cid msg as =
    runExceptT $ chat_postMessage (getConfig h) cid msg as

-------------------------------------------------------------------------------
-- Helpers

crashOnError :: MonadIO m => ExceptT T.Text m a -> m a
crashOnError x = runExceptT x >>= \case
    Left  e -> liftIO $ ioError $ userError (T.unpack e)
    Right a -> return a
