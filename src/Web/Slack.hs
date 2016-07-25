------------------------------------------
-- |
-- This module exposes functionality to write bots which responds
-- to `Event`s sent by the RTM API. By using the user state parameter `s`
-- complicated interactions can be established.
--
-- This basic example echos every message the bot recieves.
-- Other examples can be found in the
-- @<http://google.com examples>@ directory.
--
-- > myConfig :: SlackConfig
-- > myConfig = SlackConfig
-- >         { _slackApiToken = "..." -- Specify your API token here
-- >         }
-- >
-- > -- type SlackBot s = Event -> Slack s ()
-- > echoBot :: SlackBot ()
-- > echoBot (Message cid _ msg _ _ _) = sendMessage cid msg
-- > echoBot _ = return ()
-- >
-- > main :: IO ()
-- > main = runBot myConfig echoBot ()
--
module Web.Slack
    ( SlackHandle
    , withSlackHandle

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

import Web.Slack.Handle
import Web.Slack.Types
