{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Data.Time.Clock (UTCTime)
import Lentille
import Lentille.Bugzilla
import Options.Generic
import Relude
import qualified Streaming.Prelude as S

data LentilleCli w = LentilleCli
  { monocleUrl :: w ::: Text <?> "The monocle API",
    index :: w ::: Text <?> "The index name",
    crawlerName :: w ::: Text <?> "The name of the crawler",
    bugzillaUrl :: w ::: Maybe Text <?> "The bugzilla url",
    since :: w ::: Maybe String <?> "Get bugs since",
    printBugs :: w ::: Bool <?> "Just print bugs, to not amend monocle"
  }
  deriving stock (Generic)

instance ParseRecord (LentilleCli Wrapped) where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving stock instance Show (LentilleCli Unwrapped)

apiKeyEnv :: String
apiKeyEnv = "MONOCLE_API_KEY"

apiKeyEnvError :: String
apiKeyEnvError = error $ toText apiKeyEnv <> " environment not found"

readSince :: Maybe String -> Maybe UTCTime
readSince = fmap (fromMaybe (error "Could not parse time") . readMaybe)

main :: IO ()
main = do
  args <- unwrapRecord "Lentille worker"
  apiKey <- fromMaybe apiKeyEnvError <$> lookupEnv apiKeyEnv
  go args $! ApiKey (toText apiKey)
  where
    go :: LentilleCli Unwrapped -> ApiKey -> IO ()
    go args apiKey = do
      let bzUrl = fromMaybe "bugzilla.redhat.com" (bugzillaUrl args)
          sinceTSM = readSince $ since args
          sinceTS = fromMaybe (error "Couldn't parse since") sinceTSM
      bzSession <- getBugzillaSession bzUrl
      if printBugs args
        then S.mapM_ print (getBZData bzSession sinceTS)
        else withClient (monocleUrl args) Nothing $ \client ->
          run
            client
            sinceTSM
            apiKey
            (IndexName . index $ args)
            (CrawlerName . crawlerName $ args)
            (TaskDataFetcher (getBZData bzSession))
