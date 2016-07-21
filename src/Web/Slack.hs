-- | Bindings to the various Slack APIs, for writing chat bots.
--
-- > main :: IO ()
-- > main = runSlack myConfig echoBot
-- >
-- > myConfig :: SlackConfig
-- > myConfig = SlackConfig { _slackApiToken = "your API token here" }
-- >
-- > -- | For all channels of which this bot is a member, it simply watches
-- > -- for messages and echoes them back to the channel they came from.
-- > echoBot :: Slack ()
-- > echoBot = do
-- >     event <- getNextEvent
-- >     case event of
-- >         Message cid _ msg _ _ _ -> sendMessage cid msg
-- >         _ -> return ()
-- >     echoBot
--
-- Slack exposes a number of APIs which provide different (but overlapping)
-- functionality:
--
-- * The Real-Time Messaging API (<https://api.slack.com/rtm>) is
--   a WebSocket-based API that allows you to receive events from Slack in
--   real time and send basic messages.
-- * The Web API (<https://api.slack.com/web>) consists of HTTP RPC-style
--   methods. It provides support for more complex interactions with Slack,
--   such as posting messages with buttons, uploading files, making
--   reminders, etc.
--
-- This library is mostly about the RTD API. It has some very limited
-- support for using the Web API.
--
-- The functions in this library require access to a resource which must be
-- acquired: namely, a websocket connection to the slack server. Such
-- a resource is normally represented by a handle. However, the user will
-- typically be using just one handle throughout the lifetime of the
-- program, so it can be convenient to hide it in the monadic context.
-- This library supports both styles: the API from "Web.Slack.Monad" is
-- re-exported here, but you can use the API in "Web.Slack.Handle" instead
-- if you need the added flexibility.
--
-- See also: the "examples" directory for more working code.
--
module Web.Slack
    ( MonadSlack
    , Slack(..)
    , runSlack

      -- * Writing slack bots
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

import Web.Slack.Monad
import Web.Slack.Types
