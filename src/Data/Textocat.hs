module Data.Textocat (
    -- * API configuration
    Config
  , mkConfig
    -- * Error handling
  , ErrorMsg
    -- * Documents
  , Document
  , mkDocument
  , mkTaggedDocument
  , setTag
  , getText
  , getTag
    -- * Batch
  , BatchID
  , BatchState (..)
    -- ** Status returned by API
  , BatchStatus
  , getBatchID
  , getBatchState
    -- ** Content
  , Batch
  , getBatchIds
  , getDocuments
    -- * Annotated document
  , AnnotatedDocument
  , getDocumentState
  , getTag'
  , getEntities
    -- ** Document state
  , DocumentState (..)
    -- ** Entity categories
  , Category (..)
    -- ** Entity
  , Entity
  , getSpan
  , getOffsets
  , getCategory
    -- * Search results
  , SearchResult
  , getSearchQuery
  , getFoundDocuments
    -- * Service status
  , ServiceStatus
  ) where

import Data.Textocat.Internal

import Data.ByteString (ByteString)

-- | Creates a @Config@ value from API key
mkConfig :: ByteString -- ^ API key
         -> Config
mkConfig key = Config key "http://api.textocat.com"

-- | Get batch identifier from status returned by API
getBatchID :: BatchStatus -- ^ Status as returned by API
           -> BatchID
getBatchID = undefined

mkDocument = undefined

mkTaggedDocument = undefined

setTag = undefined

getText = undefined

getTag = undefined

getBatchState = undefined

getBatchIds = undefined

getDocuments = undefined

getDocumentState = undefined

getTag' = undefined

getEntities = undefined

getSpan = undefined

getOffsets = undefined

getCategory = undefined

getSearchQuery = undefined

getFoundDocuments = undefined

