{- |
Module      : Network.Textocat.Monadic
Description : Monadic binding for Textocat API
Copyright   : (c) Mansur Ziatdinov, 2014--2015
License     : Apache-2.0
Maintainer  : mz+textocat-api@fosslabs.ru
Stability   : experimental
Portability : POSIX

There are two kind of functions in this module. Functions that have apostrophe
in their names send data to server every time you call them. Functions without
them call server only if necessary (e.g. you requested it)

This monadic interface is experimental. If you know how to improve it, please
contact author
-}
{-# LANGUAGE DeriveFunctor #-}

module Network.Textocat.Monadic
       ( -- * Monad wrappers
         -- ** General monad transformer
         TextocatM
       , runTextocatM
         -- ** Monad over IO
       , Textocat
       , runTextocat
         -- * API operations with explicit calls
       , queue'
       , request'
       , retrieve'
       , search'
       , status
         -- ** Helpers
       , wait'
         -- * API operations with implicit calls
       , queue
       , queue1
       , force
       , request
       , retrieve
       ) where

import Data.Textocat
import Network.Textocat.Simple

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS
import Data.ByteString (ByteString)
import Data.Either
import Data.Text (Text)

initialSt :: [Document]
initialSt = []

-- | TextocatM is a monad transformer to use Textocat API
-- over some monad @m@ and return result @a@
newtype TextocatM m a = TextocatM
    { unTextocatM :: RWST Config () [Document] m a
    } deriving (Functor)

instance (Functor m, Monad m) => Applicative (TextocatM m) where
  pure x = TextocatM $ pure x
  TextocatM f <*> TextocatM x = TextocatM $ f <*> x

instance (Functor m, MonadIO m) => Monad (TextocatM m) where
  return = pure
  TextocatM ma >>= f = TextocatM $ ma >>= unTextocatM . f

instance MonadTrans TextocatM where
  lift action = TextocatM $ lift action

instance (Functor m, MonadIO m) => MonadIO (TextocatM m) where
  liftIO = TextocatM . liftIO

type Textocat = TextocatM IO

-- | Executes TextocatM actions with given API key
runTextocatM :: (Functor m, MonadIO m)
             => ByteString -- ^ API key
             -> TextocatM m a -- ^ TextocatM action
             -> m a
runTextocatM apiKey ma = fmap fst $ evalRWST (unTextocatM ma) (mkConfig apiKey) initialSt

-- | Executes Textocat actions with given API key
runTextocat :: ByteString -- ^ API key
            -> Textocat a -- ^ Textocat action
            -> IO a
runTextocat = runTextocatM

-- | Run function that depends on 'Config' in TextocatM monad,
-- reading 'Config' from Reader
withConfig :: (Functor m, MonadIO m)
           => (Config -> b -> IO a) -- ^ Function that depends on Config and something else
           -> b -- ^ Something else
           -> TextocatM m a
withConfig f b = TextocatM $ ask >>= liftIO . flip f b

-- | Queues given documents. No requests go to server until 'force',
-- 'request' of 'retrieve' is called
queue :: (Functor m, MonadIO m)
      => [Document] -- ^ Documents to queue
      -> TextocatM m ()
queue docs = TextocatM $ modify (docs++)

-- | Queues single document. No requests go to server until 'force',
-- 'request' of 'retrieve' is called
queue1 :: (Functor m, MonadIO m)
       => Document -- ^ Document to queue
       -> TextocatM m ()
queue1 doc = TextocatM $ modify (doc:)

-- | Queues documents. Actually, makes call to 'entityQueue' with
-- collections divided by 50 documents at most
queue' :: (Functor m, MonadIO m) => [Document] -> TextocatM m [Either ErrorMsg BatchStatus]
queue' docs | null docs = return []
            | length docs <= 50 = fmap pure $ withConfig entityQueue docs
            | otherwise = do
  let (d,rest) = splitAt 50 docs
  b <- withConfig entityQueue d
  fmap (b:) $ queue' rest

-- | Actually sends documents that were added by 'queue' and 'queue1' calls
-- to the API server.
-- If there are more than 50 documents, they will be sent in several calls,
-- each call will have 50 document at most (API limitation)
force :: (Functor m, MonadIO m) => TextocatM m [Either ErrorMsg BatchStatus]
force = TextocatM $ get >>= unTextocatM . queue'

-- | Requests batch status
request' :: (Functor m, MonadIO m)
        => BatchID -- ^ Batch
        -> TextocatM m (Either ErrorMsg BatchStatus)
request' = withConfig entityRequest

-- | Retrieves batches
retrieve' :: (Functor m, MonadIO m)
         => [BatchID] -- ^ Batches
         -> TextocatM m (Either ErrorMsg Batch)
retrieve' = withConfig entityRetrieve

-- | Search all collections
search' :: (Functor m, MonadIO m)
       => Text -- ^ Search query
       -> TextocatM m (Either ErrorMsg SearchResult)
search' = withConfig entitySearch

-- | Waits while batch processing is finished
wait' :: (Functor m, MonadIO m)
         => BatchID
         -> TextocatM m ()
wait' = withConfig waitForFinished

-- | Check service status
status :: (Functor m, MonadIO m) => TextocatM m ServiceStatus
status = TextocatM $ ask >>= liftIO . serviceStatus

-- | Request all documents that were added by 'queue' and 'queue1' calls.
-- Can create several batches, if there are more than 50 documents
request :: (Functor m, MonadIO m) => TextocatM m [Either ErrorMsg BatchStatus]
request = force >>= mapM makeRequest
  where makeRequest (Left error) = return $ Left error
        makeRequest (Right batchStatus) = request' $ getBatchID batchStatus

-- | Retrieve all documents that were added by 'queue' and 'queue1' calls.
-- Can create several batches, if there are more than 50 documents.
-- Blocks until processing of all batches is finished
retrieve :: (Functor m, MonadIO m) => TextocatM m [Either ErrorMsg Batch]
retrieve = request >>= mapM makeWait >>= uncurry (liftM2 (++)) . ret
  where
    ret = (return . map Left *** fmap pure . retrieve') . partitionEithers
    makeWait (Left error) = return $
        Left error
    makeWait (Right batchStatus) = let bId = getBatchID batchStatus
        in wait' bId >> return (Right bId)
