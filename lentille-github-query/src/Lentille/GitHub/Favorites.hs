{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Get user's favorites:
-- > $ cabal repl
-- > λ> import Lentille.GitHub.Favorites
-- > λ> xs <- S.toList_ $ (newGithubGraphClient "https://api.github.com/graphql" >>= \client -> getFavoritesStream client "TristanCacqueray")
-- > [github-graphql] got 1460 hasNextPage  ratelimit 1/4999 reset at: 2021-05-04T00:00:17Z
-- > ...
-- > λ> xs
-- > [ UserStarredRepositoriesEdgesNodeRepository {
--       nameWithOwner = "morpheusgraphql/morpheus-graphql",
--       stargazerCount = 298,
--       updatedAt = DateTime "2021-05-03T21:00:22Z",
--       description = Just "Haskell GraphQL Api, Client and Tools"
--     },
-- > , ...
-- > ]
module Lentille.GitHub.Favorites where

import Data.Morpheus.Client
import Lentille.GitHub (GitHubGraphClient, PageInfo (..), RateLimit (..), schemaLocation, streamFetch)
import Relude
import Streaming (Of, Stream)

newtype DateTime = DateTime Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

defineByDocumentFile
  schemaLocation
  [gql|
    query GetFavorites ($userName: String!, $cursor: String!)
    {
      rateLimit {
        used
        remaining
        resetAt
      }
      user(login: $userName) {
        starredRepositories(
          first: 100,
          after: $cursor,
          orderBy: {direction: ASC, field: STARRED_AT}
        ) {
          totalCount
          pageInfo {hasNextPage endCursor}
          edges {
            node {
              nameWithOwner
              stargazerCount
              updatedAt
              description
            }
          }
        }
      }
    }
  |]

type UserFavorite = UserStarredRepositoriesEdgesNodeRepository

getFavoritesStream ::
  MonadIO m =>
  GitHubGraphClient ->
  Text ->
  Stream (Of UserFavorite) m ()
getFavoritesStream client username = streamFetch client mkArgs transformResponse
  where
    mkArgs cursor' = GetFavoritesArgs (toString username) (toString cursor')
    transformResponse :: GetFavorites -> (PageInfo, RateLimit, [UserFavorite])
    transformResponse resp = case resp of
      ( GetFavorites
          (Just (RateLimitRateLimit used' remaining' (DateTime resetAt')))
          ( Just
              ( UserUser
                  ( UserStarredRepositoriesStarredRepositoryConnection
                      totalCount'
                      (UserStarredRepositoriesPageInfoPageInfo hasNextPage' endCursor')
                      (Just xs)
                    )
                )
            )
        ) ->
          ( PageInfo hasNextPage' endCursor' totalCount',
            RateLimit used' remaining' resetAt',
            map getNode $ catMaybes xs
          )
      respOther -> error ("Invalid response: " <> show respOther)
    getNode (UserStarredRepositoriesEdgesStarredRepositoryEdge node') = node'
