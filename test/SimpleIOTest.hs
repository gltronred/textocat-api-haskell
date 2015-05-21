{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit
import Test.Tasty
import Test.Tasty.HUnit

import Data.Textocat
import Network.Textocat.Simple

import Data.Either

cfg = mkConfig "API-KEY"

-- | We cannot test too much actually, and we don't need to.
-- Haskell takes care about types. All we need is to test that
-- Textocat API is consistent with implemented Read/FromJSON
-- and Show/ToJSON instances
main :: IO ()
main = defaultMain tests

tests = testGroup "Textocat API"
        [ serviceStatusTest
        , workflowTest
        ]

serviceStatusTest = testCase "Textocat Status 200" $ do
  status <- serviceStatus cfg
  status @?= ServiceOK

workflowTest = testCaseSteps "Test workflow" $ \step -> do
  step "Queue documents"
  ebs1 <- entityQueue cfg docs
  assertBool "Batch was not queued" $ isRight ebs1
  let Right bs1 = ebs1

  step "Request status"
  ebs2 <- entityRequest cfg $ getBatchID bs1
  assertBool "Batch was not requested" $ isRight ebs2
  let Right bs2 = ebs2
  assertEqual "Request returned other Batch" (getBatchID bs2) (getBatchID bs1)

  step "Wait until finished"
  waitForFinished cfg $ getBatchID bs2

  step "Retrieve documents"
  -- we retrieve both bs1 and bs2 to test whether API works correctly
  -- with several documents
  eres <- entityRetrieve cfg $ map getBatchID [bs1, bs2]
  assertBool "Batch was not retrieved" $ isRight eres
  let Right res = eres
      ads = getDocuments res

  step "Check entities retrieved"
  -- cannot check. There are no standard examples with known categories

  step "Search documents"
  efound <- entitySearch cfg "Путин"
  assertBool "Search failed" $ isRight efound
  let Right found = efound
  assertBool "We found nothing" $ not $ null $ getFoundDocuments found
  

docs = map (\t -> mkTaggedDocument t "test")
       [ "Путин"
       , "ООО \"Рога и копыта\""
       , "Уганда"
       , "Таганрог"
       , "Мордор" -- FACILITY???
       , "прошлое лето"
       , "100500 долларов"
       ]

