{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Lentille
import Lentille.BugzillaMock
import Lentille.MonocleMock
import Network.HTTP.Mock (withMockedManager)
import Relude
import Test.Tasty
import Test.Tasty.HUnit
import qualified Web.Bugzilla.RedHat as BZ

main :: IO ()
main = defaultMain (testGroup "Tests" [workerTests, bzClientTests, monocleClientTests])

workerTests :: TestTree
workerTests =
  testGroup
    "Lentille.Worker"
    [testRun]

testRun :: TestTree
testRun = testCase "run" go
  where
    go = withMockClient $ \client -> do
      bzSession <- bugzillaMockClient
      run
        client
        (ApiKey "fake")
        (IndexName "openstack")
        (CrawlerName "lentille")
        (TrackerDataFetcher (getBZData bzSession))

bzClientTests :: TestTree
bzClientTests =
  testGroup
    "BugzillaMock"
    [testSearchBugs, testGetBug]

testGetBug :: TestTree
testGetBug = testCase "getBug" go
  where
    go = do
      bzSession <- bugzillaMockClient
      Just bug' <- BZ.getBug bzSession 1791815
      -- print bug'
      assertBool "Got bug ids" (isJust $ BZ.bugExternalBugs bug')

testBugToTrackerData :: TestTree
testBugToTrackerData = testCase "bugToTrackerData" go
  where
    go = do
      bzSession <- bugzillaMockClient
      Just bz <- BZ.getBug bzSession 1791815
      case toTrackerData bz of
        (td : _tds) ->
          sequence_
            [ tdIssueId td @=? 1791815,
              tdChangeUrl td @=? "https://review.opendev.org/1717044",
              tdIssueUrl td @=? "https://bugzilla.redhat.com/show_bug.cgi?id=1791815"
            ]
        [] -> assertBool "No external bugs found" False

testSearchBugs :: TestTree
testSearchBugs = testCase "searchBugs" go
  where
    sinceTS = fromMaybe (error "Oops") $ readMaybe "2021-04-01 00:00:00 UTC"
    go = do
      bzSession <- bugzillaMockClient
      bugs <- BZ.searchBugsAll bzSession (searchExpr sinceTS)
      -- print (length $ bugs)
      -- print (head <$> nonEmpty bugs)
      assertBool "Got bugs" (not . null $ bugs)

monocleClientTests :: TestTree
monocleClientTests =
  testGroup
    "Lentille.Client"
    [ testGetIndices,
      testGetUpdatedSince,
      testBugToTrackerData,
      testPostData
    ]

withMockClient :: (MonocleClient -> IO ()) -> IO ()
withMockClient cb = withMockedManager monocleMockApplication go
  where
    go manager = withClient "http://localhost" (Just manager) cb

testGetUpdatedSince :: TestTree
testGetUpdatedSince = testCase "getUpdatedSince" go
  where
    go = withMockClient $ \client -> do
      lastUpdated <- getUpdatedSince client (IndexName "test") (CrawlerName "test")
      putText (show lastUpdated)
      assertBool "Got update" True

testGetIndices :: TestTree
testGetIndices = testCase "getIndices" go
  where
    go = withMockClient $ \client -> do
      indices <- getIndices client
      assertBool "Got indicies" (indices == ["indice1", "indice2"])

testPostData :: TestTree
testPostData = testCase "postData" go
  where
    go = withMockClient $ \client -> do
      res <- postTrackerData client (IndexName "test") (CrawlerName "test") (ApiKey "failme") []
      assertEqual "Call failed" ["42"] res
