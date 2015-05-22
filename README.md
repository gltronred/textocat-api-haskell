Textocat API -- Haskell
=======================

Unofficial Haskell binding for Textocat API <http://textocat.com>

Installation
------------

You can download this package from [http://hackage.haskell.org/package/textocat-api](hackage)

```bash
cabal install textocat-api
```


Documentation
-------------

API itself is documented at <http://docs.textocat.com>

This binding is documented at <http://hackage.haskell.org/package/textocat-api>


Usage
-----

```haskell
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
```


License
-------

Copyright 2014-2015 Mansur Ziatdinov

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

