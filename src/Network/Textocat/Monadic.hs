{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Network.Textocat.Monadic
       ( TextocatM
       , Textocat
       , runTextocatM
       , runTextocat
       , queue
       , queue1
       , force
       , request
       , retrieve
       , search
       , status
       ) where

import Data.Textocat
import Network.Textocat.Simple

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS
import Data.ByteString (ByteString)
import Data.Text (Text)

initialSt :: [Document]
initialSt = []

newtype TextocatM m a = TextocatM
    { unTextocatM :: RWST Config () [Document] m a
    } deriving (Functor)

instance (Functor m, Monad m) => Applicative (TextocatM m) where
  pure x = TextocatM $ pure x
  TextocatM f <*> TextocatM x = TextocatM $ f <*> x

instance (Functor m, MonadIO m) => Monad (TextocatM m) where
  return = pure
  TextocatM ma >>= f = TextocatM $ ma >>= unTextocatM . f

instance (Functor m, MonadIO m) => MonadIO (TextocatM m) where
  liftIO = TextocatM . liftIO

type Textocat = TextocatM IO

runTextocatM :: (Functor m, MonadIO m) => ByteString -> TextocatM m a -> m a
runTextocatM apiKey ma = fmap fst $ evalRWST (unTextocatM ma) (mkConfig apiKey) initialSt

runTextocat :: ByteString -> Textocat a -> IO a
runTextocat = runTextocatM

withConfig :: (Functor m, MonadIO m) => (Config -> b -> IO a) -> b -> TextocatM m a
withConfig f b = TextocatM $ ask >>= liftIO . flip f b

queue :: (Functor m, MonadIO m) => [Document] -> TextocatM m ()
queue docs = TextocatM $ modify (docs++)

queue1 :: (Functor m, MonadIO m) => Document -> TextocatM m ()
queue1 doc = TextocatM $ modify (doc:)

queue50 :: (Functor m, MonadIO m) => [Document] -> TextocatM m [Either ErrorMsg BatchStatus]
queue50 docs | null docs = return []
             | length docs <= 50 = fmap pure $ withConfig entityQueue docs
             | otherwise = do
  let (d,rest) = splitAt 50 docs
  b <- withConfig entityQueue d
  fmap (b:) $ queue50 rest
     
force :: (Functor m, MonadIO m) => TextocatM m [Either ErrorMsg BatchStatus]
force = TextocatM $ get >>= unTextocatM . queue50

request :: (Functor m, MonadIO m) => BatchID -> TextocatM m (Either ErrorMsg BatchStatus)
request = withConfig entityRequest

retrieve :: (Functor m, MonadIO m) => [BatchID] -> TextocatM m (Either ErrorMsg Batch)
retrieve = withConfig entityRetrieve

search :: (Functor m, MonadIO m) => Text -> TextocatM m (Either ErrorMsg SearchResult)
search = withConfig entitySearch

status :: (Functor m, MonadIO m) => TextocatM m ServiceStatus
status = TextocatM $ ask >>= liftIO . serviceStatus

