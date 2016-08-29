{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Class
import Data.Maybe
import Pipes
import System.Environment
import Web.Slack

main :: IO ()
main = do
    conf <- mkConfig
    runSlack conf pipeBot

mkConfig :: IO SlackConfig
mkConfig = do
    x <- lookupEnv "SLACK_API_TOKEN"
    let apiToken = fromMaybe (error "SLACK_API_TOKEN not set") x
    return SlackConfig{ _slackApiToken = apiToken }

pipeBot :: Slack ()
pipeBot = runEffect $ slackProducer >-> slackConsumer

slackProducer :: MonadSlack m => Producer Event m ()
slackProducer = forever $ lift getNextEvent >>= yield

slackConsumer :: MonadSlack m => Consumer Event m ()
slackConsumer = forever $ do
    await >>= \case
        (Message cid _ msg _ _ _) -> lift $ sendMessage cid msg
        _ -> return ()
