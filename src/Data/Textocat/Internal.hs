{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Textocat.Internal where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString (ByteString)
import           Data.Char
import qualified Data.Text as T
import           Data.Text (Text)

-- | Error message
type ErrorMsg = String

-- | API configuration. Currently only API key is saved
data Config = Config
    { apiKey :: ByteString -- ^ API key
    , hostName :: String -- ^ host name
    } deriving (Eq,Show)

-- | Document sent for analysis
data Document = Document
    { td_text :: Text -- ^ Text (less than 100 KB)
    , td_tag :: Maybe Text -- ^ Tag (say, user identifier of document)
    } deriving (Eq,Show)

$(deriveJSON
      (defaultOptions
       { fieldLabelModifier = drop 3
       , constructorTagModifier = map toLower
       , omitNothingFields = True
       })
      ''Document)

-- | Batch identifier (you get it after submitting documents)
newtype BatchID = BatchID
    { tr_batch_id :: Text
    } deriving (Eq,Show,Read)

instance FromJSON BatchID where
  parseJSON (String t) = return $ read $ T.unpack t
  parseJSON _ = error "Expected string while reading status"

instance ToJSON BatchID where
  toJSON x = String $ T.pack $ show x

-- | States of batch processing
data BatchState
    = FINISHED
    | IN_PROGRESS
    deriving (Eq,Show,Read)

instance FromJSON BatchState where
  parseJSON (String t) = return $ read $ T.unpack t
  parseJSON _ = error "Expected string while reading status"

instance ToJSON BatchState where
  toJSON x = String $ T.pack $ show x

-- | Status of batch
data BatchStatus = BatchStatus
    { ts_batchId :: Text -- ^ Batch ID
    , ts_status :: BatchState -- ^ Batch status
    } deriving (Eq,Show)

$(deriveJSON
      (defaultOptions
       { fieldLabelModifier = drop 3
       , constructorTagModifier = map toLower
       })
      ''BatchStatus)

-- | States of document processing
data DocumentState
    = SUCCESS
    | INPUT_ERROR
    | SERVICE_ERROR
    deriving (Eq,Show,Read)

instance FromJSON DocumentState where
  parseJSON (String t) = return $ read $ T.unpack t
  parseJSON _ = error "Expected string while reading status"

instance ToJSON DocumentState where
  toJSON x = String $ T.pack $ show x

-- | Entity category
data Category
    = Person
    | Organization
    | GPE
    | Location
    | Facility
    | Time
    | Money
    deriving (Eq,Ord,Show,Read)

instance FromJSON Category where
  parseJSON (String t) = return $ read $ T.unpack t
  parseJSON _ = error "Expected string when reading category"

instance ToJSON Category where
  toJSON x = String $ T.pack $ show x

data Entity = Entity
    { te_span :: Text
    , te_beginOffset :: Int
    , te_endOffset :: Int
    , te_category :: Category
    } deriving (Eq,Show)

$(deriveJSON
      (defaultOptions
       { fieldLabelModifier = drop 3
       , constructorTagModifier = map toLower
       })
      ''Entity)

data AnnotatedDocument = AnnotatedDocument
    { te_status :: DocumentState
    , te_tag :: Maybe Text
    , te_entities :: [Entity]
    } deriving (Eq,Show)

$(deriveJSON
      (defaultOptions
       { fieldLabelModifier = drop 3
       , constructorTagModifier = map toLower
       , omitNothingFields = True
       })
      ''AnnotatedDocument)

data Batch = Batch
    { ta_batchIds :: [BatchID]
    , ta_documents :: [AnnotatedDocument]
    } deriving (Eq,Show)

$(deriveJSON
      (defaultOptions
       { fieldLabelModifier = drop 3
       , constructorTagModifier = map toLower
       })
      ''Batch)

data SearchResult = SearchResult
    { sr_searchQuery :: Text
    , sr_documents :: [AnnotatedDocument]
    } deriving (Eq,Show)

data ServiceStatus
    = ServiceOK
    | ServiceUnavailable
    deriving (Eq,Show)

