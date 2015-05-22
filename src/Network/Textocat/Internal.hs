{-# LANGUAGE OverloadedStrings #-}

module Network.Textocat.Internal where

import           Data.Textocat.Internal

import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char
import           Data.Conduit
import           Data.Maybe
import qualified Data.Text as T
import           Network.HTTP.Conduit
import           Network.HTTP.Types

-- | Make call to API endpoint, send parameter and get answer
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
    -- TODO: insert response status handling here
    BS.putStrLn $ responseBody resp
    return $ eitherDecode $ responseBody resp

-- | Check service status. We cannot do it usual way by calling 'makeConn'
checkServiceStatus :: Config -- ^ Config
                   -> IO ServiceStatus
checkServiceStatus (Config apiKey urlHead) = do
  req' <- parseUrl $ urlHead ++ "/status"
  let params = [("auth_token", Just apiKey)]
      req = setQueryString params $ req' { checkStatus = \_ _ _ -> Nothing
                                         , method = renderStdMethod GET
                                         , requestHeaders = [("Content-Type", "application/json")]}
  resp <- withManager $ httpLbs req
  return $ case statusCode $ responseStatus resp of
    200 -> ServiceOK
    503 -> ServiceUnavailable

