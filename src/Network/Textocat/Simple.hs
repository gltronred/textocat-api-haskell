{-# LANGUAGE OverloadedStrings #-}

module Network.Textocat.Simple (
    -- * Entity API calls
    entityQueue
  , entityRequest
  , entityRetrieve
  , entitySearch
    -- * Status API call
  , serviceStatus
    -- * Helper functions
  , waitForFinished
  , queueRetrieve
  ) where

import Data.Textocat
import Data.Textocat.Internal
import Network.Textocat.Internal

import           Control.Applicative
import           Control.Concurrent
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text.Encoding as E
import           Network.HTTP.Types

-- * Simple API requests

fromBatchId :: BatchID -> B.ByteString
fromBatchId = E.encodeUtf8 . batch_id

entityQueue :: Config -> [Document] -> IO (Either ErrorMsg BatchStatus)
entityQueue cfg = makeConn cfg "/entity/queue" POST "body" . pure . BS.toStrict . encode

entityRequest :: Config -> BatchID -> IO (Either ErrorMsg BatchStatus)
entityRequest cfg = makeConn cfg "/entity/request" GET "batch_id" . pure . fromBatchId

entityRetrieve :: Config -> [BatchID] -> IO (Either ErrorMsg Batch)
entityRetrieve cfg = makeConn cfg "/entity/retrieve" GET "batch_id" . map fromBatchId

entitySearch :: Config -> Text -> IO (Either ErrorMsg SearchResult)
entitySearch cfg = makeConn cfg "/entity/search" GET "search_query" . pure . E.encodeUtf8

-- * Helpers

waitForFinished :: Config -> BatchID -> IO ()
waitForFinished cfg batch = do
  mtres <- entityRequest cfg batch
  case mtres of
    Left err -> error $ "Something went wrong: " ++ err
    Right tres -> let status = bs_status tres
                  in if status == FINISHED
                     then return ()
                     else do
                       threadDelay 100000
                       waitForFinished cfg batch

queueRetrieve :: Config -> [Document] -> IO [[Entity]]
queueRetrieve cfg docs = do
  ebatch <- entityQueue cfg docs
  case ebatch of
    Left err -> return []
    Right batch -> do
      let id = getBatchID batch
      waitForFinished cfg id
      fmap (either (const []) (map ad_entities . b_documents)) $ entityRetrieve cfg [id]

serviceStatus :: Config -> IO ServiceStatus
serviceStatus cfg = checkServiceStatus cfg

