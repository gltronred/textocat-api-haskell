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
  , delTag
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
  , getDocumentTag
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
  , ServiceStatus (..)
  ) where

import Data.Textocat.Internal

import Control.Arrow
import Data.ByteString (ByteString)
import Data.Text (Text)

-- | Creates a @Config@ value from API key
mkConfig :: ByteString -- ^ API key
         -> Config
mkConfig key = Config key "http://api.textocat.com"

-- | Get batch identifier @BatchID@ from status returned by API
getBatchID :: BatchStatus -- ^ Status as returned by API
           -> BatchID
getBatchID = BatchID . bs_batchId

-- | Create a @Document@ without tag from text
mkDocument :: Text -- ^ Text
           -> Document
mkDocument t = Document t Nothing

-- | Create a tagged @Document@ from text and tag
mkTaggedDocument :: Text -- ^ Text
                 -> Text -- ^ Tag
                 -> Document
mkTaggedDocument text tag = Document text $ Just tag

-- | Replace or set a tag
setTag :: Text -- ^ Tag 
       -> Document -- ^ Document
       -> Document
setTag tag doc = doc { d_tag = Just tag }

-- | Delete tag
delTag :: Document -> Document
delTag doc = doc { d_tag = Nothing }

-- Following functions are not needed, IMHO
--
-- -- | Get text from document
-- getText :: Document -> Text
-- getText = d_text

-- -- | Get tag from document
-- getTag :: Document -> Text
-- getTag = d_tag

-- | Get batch status from request call result
getBatchState :: BatchStatus -- ^ Request call result
              -> BatchState
getBatchState = bs_status

-- | Get batch ids from retrieve call result
getBatchIds :: Batch -- ^ Batch
            -> [BatchID]
getBatchIds = b_batchIds

-- | Get annotated documents from retrieve call result
getDocuments :: Batch -- ^ Batch
             -> [AnnotatedDocument]
getDocuments = b_documents

-- | Get annotated document state
getDocumentState :: AnnotatedDocument -- ^ Document
                 -> DocumentState
getDocumentState = ad_status

-- | Get annotated document tag
getDocumentTag :: AnnotatedDocument -- ^ Document
               -> Maybe Text
getDocumentTag = ad_tag

-- | Get entities from annotated document
getEntities :: AnnotatedDocument -- ^ Document
            -> [Entity]
getEntities = ad_entities

-- | Get entity span
getSpan :: Entity -- ^ Entity
        -> Text
getSpan = e_span

-- | Get pair of start and end offsets
getOffsets :: Entity -- ^ Entity
           -> (Int, Int)
getOffsets = e_beginOffset &&& e_endOffset

-- | Get entity category
getCategory :: Entity -- ^ Entity
            -> Category
getCategory = e_category

-- | Get search query
getSearchQuery :: SearchResult -- ^ Search call result
               -> Text
getSearchQuery = sr_searchQuery

-- | Get found documents
getFoundDocuments :: SearchResult -- ^ Search call result
                  -> [AnnotatedDocument]
getFoundDocuments = sr_documents

