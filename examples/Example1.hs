{-# LANGUAGE OverloadedStrings #-}

import Data.Textocat
import Network.Textocat

import Control.Monad (when)
import System.Exit (exitFailure)

cfg = mkConfig "API-KEY"

documents = [ mkDocument "Привет, мир!"
            , setTag "haskell" $ mkDocument "Язык Haskell признан лучшим языком для выдающихся хакеров на ICFPC 2014"
            ]

main = do
  status <- serviceStatus cfg
  when (status /= ServiceOK) exitFailure

  putStrLn "Queue documents"
  Right bs <- entityQueue cfg documents

  putStrLn "Request status"
  entityRequest cfg $ getBatchID bs

  putStrLn "Wait until finished"
  waitForFinished cfg $ getBatchID bs

  putStrLn "Retrieve documents"
  -- we retrieve both bs1 and bs2 to test whether API works correctly
  -- with several documents
  Right res <- entityRetrieve cfg $ [getBatchID bs]
  let entities = concatMap getEntities $ getDocuments res

  mapM_ print entities

  putStrLn "Search documents"
  Right found <- entitySearch cfg "Haskell"

  mapM_ print $ getFoundDocuments found
  
