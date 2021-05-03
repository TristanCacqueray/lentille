{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A github graphql query layer built on top of the morpheus-graphql-client
--
-- Make sure to have a copy of the github schema first:
--
-- > $ curl -o github.graphql -L https://docs.github.com/public/schema.docs.graphql
--
-- Here is an example usages:
--
-- > $ GITHUB_GRAPH_TOKEN=your-token cabal repl
-- > λ> client <- newGithubGraphClient "https://api.github.com/graphql"
-- > λ> fetchIssues client (GetIssuesArgs "repo:change-metrics/monocle linked:pr updated:>=2021-04-01")
-- > Right (GetIssues {search = SearchSearchResultItemConnection {nodes = Just [Just (SearchNodesIssue {title = "[spec] Use an interface ...
module Lentille.GitHubQuery where

import qualified Data.ByteString.Lazy as LBS
import Data.Morpheus.Client
import Network.HTTP.Client (Manager, RequestBody (RequestBodyLBS), httpLbs, method, newManager, parseRequest_, requestBody, requestHeaders, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Relude

newtype DateTime = DateTime Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

newtype URI = URI Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

-- | The defineByDocumentFile is a template haskell quasi quoter thing.
-- It generates data types for the graphql query.
-- To see the generated code, use the `-ddump-splices` option, for example using a REPL:
--
-- > $ cabal repl -O0
-- > λ> :set -ddump-splices
-- > λ> :load src/Lentille/GraphQLTest.hs
-- > [...]
--
-- Restart the REPL to disable the option.
defineByDocumentFile
  "./github.graphql"
  [gql|
    query GetIssues ($queryString: String!)
    {
        search(query: $queryString, type: ISSUE, first: 25) {
            nodes {
                ... on Issue {
                    title
                    updatedAt
                    timelineItems(
                      first: 100,
                      itemTypes: [CONNECTED_EVENT],
                    ) {
                      nodes {
                        ... on ConnectedEvent {
                          subject {
                            ... on PullRequest {
                              url
                            }
                          }
                        }
                      }
                   }
                }
            }
        }
    }
  |]

data GitHubGraphClient = GitHubGraphClient
  { manager :: Manager,
    url :: Text,
    token :: Text
  }

newGithubGraphClient :: MonadIO m => Text -> m GitHubGraphClient
newGithubGraphClient url = do
  manager <- liftIO $ newManager tlsManagerSettings
  token <-
    toText
      . fromMaybe (error "GITHUB_GRAPH_TOKEN environment is missing")
      <$> liftIO (lookupEnv "GITHUB_GRAPH_TOKEN")
  pure $ GitHubGraphClient manager url token

runGithubGraphRequest :: MonadIO m => GitHubGraphClient -> LBS.ByteString -> m LBS.ByteString
runGithubGraphRequest (GitHubGraphClient manager url token) jsonBody = do
  putTextLn $ "Sending this query: " <> decodeUtf8 jsonBody
  let initRequest = parseRequest_ (toString url)
      request =
        initRequest
          { method = "POST",
            requestHeaders =
              [ ("Authorization", "token " <> encodeUtf8 token),
                ("User-Agent", "change-metrics/lentille-morpheus")
              ],
            requestBody = RequestBodyLBS jsonBody
          }
  response <- liftIO $ httpLbs request manager
  -- print response
  pure (responseBody response)

fetchIssues :: MonadIO m => GitHubGraphClient -> GetIssuesArgs -> m (Either String GetIssues)
fetchIssues client = fetch (runGithubGraphRequest client)
