{-# LANGUAGE OverloadedStrings #-}

module Network.Textocat.Internal where

import           Data.Textocat.Internal

import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char
import           Data.Conduit
import           Data.Maybe
import qualified Data.Text as T
import           Network.HTTP.Conduit
import           Network.HTTP.Types

apiKey = "f9e1c6d38ef32f60bdcbd89326b502c6"

-- TODO: ask to fix bug
fixSpaces :: BS.ByteString -> BS.ByteString
fixSpaces = go True
  where go flag str = case BS.uncons str of
          Nothing -> BS.empty
          Just (b,bs) -> if flag && isSpace b
                         then go flag bs
                         else if b == '"'
                              then b `BS.cons` go (not flag) bs
                              else b `BS.cons` go flag bs

makeConn :: (FromJSON b)
         => Config          -- ^ API config
         -> String          -- ^ URL endpoint
         -> StdMethod       -- ^ HTTP method
         -> B.ByteString    -- ^ parameter name; @"body"@ means send in request body
         -> [B.ByteString]  -- ^ parameter values
         -> IO (Either ErrorMsg b)
makeConn (Config apiKey urlHead) urlTail mtd param infos = do
    req' <- parseUrl $ urlHead ++ urlTail
    let params = [("auth_token", Just apiKey)] ++
            (if param /= "body"
                 then map (\info -> (param, Just info)) infos
                 else [])
        body = if param == "body"
               then head infos
               else ""
        req = setQueryString params $
            req'
            { checkStatus = \_ _ _ ->
                                 Nothing
            , requestBody = RequestBodyBS body
            , method = renderStdMethod mtd
            , requestHeaders = [("Content-Type", "application/json")]
            }
    print req
    resp <- withManager $ httpLbs req
    print $ responseStatus resp
    -- TODO: insert response status here
    print $ fixSpaces $ responseBody resp
    return $ eitherDecode $ fixSpaces $ responseBody resp
