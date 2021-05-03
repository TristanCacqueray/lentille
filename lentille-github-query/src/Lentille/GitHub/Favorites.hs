{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | 

module Lentille.GitHub.Favorites where

import qualified Data.ByteString.Lazy as LBS
import Data.Morpheus.Client
import Relude

newtype DateTime = DateTime Text deriving (Show, Eq, EncodeScalar, DecodeScalar)

defineByDocumentFile
  "./github.graphql"
  [gql|
    query GetFavorites ($userName: String!, $cursor: String!)
    {
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

