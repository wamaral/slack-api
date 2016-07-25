{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

-- | The monadic API. You may find the handle-based API (found in
-- "Web.Slack.Handle") more flexible.
--
-- > main :: IO ()
-- > main = runSlack myConfig echobot
-- >
-- > echobot :: Slack ()
-- > echobot = forever $ getNextEvent >>= \case
-- >     Message cid _ msg _ _ _ -> sendMessage cid msg
-- >     _ -> return ()
--
module Web.Slack.Monad
    ( MonadSlack(..)
    , Slack(..)
    , runSlack

      -- * Monadic API
    , getNextEvent
    , getSession
    , getConfig
    , sendPing
    , sendMessage
    , sendRichMessage

      -- * Converting between monadic and handle-based
    , monadicToHandled
    , handledToMonadic

      -- * Types
    , SlackConfig(..)
    , SlackSession(..)
    , Event(..)
    , module Web.Slack.Types
    ) where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.List
import qualified Control.Monad.RWS.Lazy   as L
import qualified Control.Monad.RWS.Strict as S
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy   as L
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy   as L
import qualified Control.Monad.Writer.Strict as S
import qualified Data.Text as T
import qualified Web.Slack.Handle as H
import Web.Slack.Types
import Web.Slack.WebAPI

class (MonadIO m) => MonadSlack m where
    askSlackHandle :: m H.SlackHandle

instance MonadSlack Slack where
    askSlackHandle = Slack ask

#define LIFT_SLACK where askSlackHandle = lift $ askSlackHandle
instance (MonadSlack m)           => MonadSlack (IdentityT m)    LIFT_SLACK
instance (MonadSlack m)           => MonadSlack (MaybeT m)       LIFT_SLACK
instance (MonadSlack m)           => MonadSlack (ListT m)        LIFT_SLACK
instance (MonadSlack m)           => MonadSlack (ExceptT e m)    LIFT_SLACK
instance (MonadSlack m)           => MonadSlack (ContT r m)      LIFT_SLACK
instance (MonadSlack m)           => MonadSlack (ReaderT r m)    LIFT_SLACK
instance (MonadSlack m)           => MonadSlack (S.StateT s m)   LIFT_SLACK
instance (MonadSlack m)           => MonadSlack (L.StateT s m)   LIFT_SLACK
instance (MonadSlack m, Monoid w) => MonadSlack (S.WriterT w m)  LIFT_SLACK
instance (MonadSlack m, Monoid w) => MonadSlack (L.WriterT w m)  LIFT_SLACK
instance (MonadSlack m, Monoid w) => MonadSlack (S.RWST r w s m) LIFT_SLACK
instance (MonadSlack m, Monoid w) => MonadSlack (L.RWST r w s m) LIFT_SLACK

-- I decided to make 'Slack' opaque in order to avoid stealing 'MonadReader'.
-- So, for instance, it's now possible for the user to use @ReaderT Foo
-- Stack ()@ whilst still having it be an instance of MonadSlack.
newtype Slack a = Slack (ReaderT H.SlackHandle IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

runSlack :: SlackConfig -> Slack a -> IO a
runSlack conf (Slack x) = H.withSlackHandle conf (runReaderT x)

monadicToHandled :: Slack a -> H.SlackHandle -> IO a
monadicToHandled (Slack x) h = runReaderT x h

handledToMonadic :: (H.SlackHandle -> IO a) -> Slack a
handledToMonadic fn = liftIO . fn =<< Slack ask

-------------------------------------------------------------------------------
-- Public API

-- | Returns the next event in the queue. If the queue is empty, blocks
-- until a new event is recieved.
getNextEvent :: MonadSlack m => m Event
getNextEvent = liftIO . H.getNextEvent =<< askSlackHandle

-- | When the connection is established, the slack server sends a bunch of
-- session information. This is accessible here.
--
-- (Caveat: this information represents the state as of the beginning of
-- the session; it is liable to become stale. If you care about keeping an
-- up-to-date view of this stuff, you need to track changes to it using
-- 'getNextEvent'.)
getSession :: MonadSlack m => m SlackSession
getSession = H.getSession <$> askSlackHandle

-- | Retrieve the config used to initiate the session.
getConfig :: MonadSlack m => m SlackConfig
getConfig = H.getConfig <$> askSlackHandle

-- | Send a ping to the server, which should respond by sending a 'Pong'
-- event.
sendPing :: MonadSlack m => m ()
sendPing = liftIO . H.sendPing =<< askSlackHandle

-- | Post a simple message to the specified channel. From the docs:
--
-- * Clients should limit messages sent to channels to 4000 characters.
-- * Clients should not send more than one message per second sustained.
sendMessage :: MonadSlack m => ChannelId -> T.Text -> m ()
sendMessage cid msg = askSlackHandle >>= \h -> liftIO (H.sendMessage h cid msg)

-- | Post a complex message using the web API. There's a lot more
-- functionality than is exposed here - see
-- <https://api.slack.com/methods/chat.postMessage>.
sendRichMessage
    :: MonadSlack m => ChannelId -> T.Text -> [Attachment] -> m (Either T.Text ())
sendRichMessage cid msg as = do
    h <- askSlackHandle
    liftIO $ H.sendRichMessage h cid msg as
