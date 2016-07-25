{-# LANGUAGE LambdaCase #-}

module Tests.ConnectionTest (main) where

import Data.Maybe
import System.Environment
import Web.Slack
import qualified Web.Slack.Handle as H

main :: IO ()
main = do
    conf <- mkConfig
    runSlack conf inertBot1
    H.withSlackHandle conf inertBot2

mkConfig :: IO SlackConfig
mkConfig = do
    x <- lookupEnv "SLACK_API_TOKEN"
    let apiToken = fromMaybe (error "SLACK_API_TOKEN not set") x
    return SlackConfig{ _slackApiToken = apiToken }

inertBot1 :: Slack ()
inertBot1 =
    getNextEvent >>= \case
        Hello -> return ()
        e -> error ("Unexpected event: " ++ show e)

inertBot2 :: H.SlackHandle -> IO ()
inertBot2 h =
    H.getNextEvent h >>= \case
        Hello -> return ()
        e -> error ("Unexpected event: " ++ show e)
