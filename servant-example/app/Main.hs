{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (..))
import Jordan.Servant
import Jordan.Servant.Client
import Jordan.Servant.Example
import Jordan.Servant.Example.ServerM
import Network.HTTP.Client hiding (Proxy)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Servant.API
import Servant.Client
import Servant.OpenApi (toOpenApi)
import Jordan.Servant.OpenApi
import Servant.Server (Handler, Server, hoistServer, serve)
import System.Environment

apiStr = encode $ toOpenApi (Proxy :: Proxy API)

toHandler :: IO a -> Handler a
toHandler = liftIO

main = do
  args <- getArgs
  if "client" `elem` args
    then clientMain
    else serverMain

makePerson ::
  CreatePerson ->
  ClientM
    ( Union
        '[ WithStatus 200 (ViaJordan Person),
           WithStatus
             400
             (ViaJordan CreatePersonErrors)
         ]
    )
getPerson :: Maybe QueryFilter -> ClientM (ViaJordan [Person])
makePerson :<|> getPerson = client (Proxy :: Proxy API)

print' :: (MonadIO m, Show a) => a -> m ()
print' = liftIO . print

testM = do
  makePerson (CreatePerson "Bob" "Smith") >>= print'
  makePerson (CreatePerson "" "Smith") >>= print'
  makePerson (CreatePerson "Joe" "Smith") >>= print'
  getPerson Nothing >>= print'
  getPerson (Just $ MkQueryFilter (Just "B") Nothing) >>= print'
  getPerson (Just $ MkQueryFilter Nothing (Just "Smi")) >>= print'

clientMain :: IO ()
clientMain = do
  r <- newManager defaultManagerSettings
  url <- parseBaseUrl "http://localhost:8000"
  runClientM testM (mkClientEnv r url) >>= print

serverMain :: IO ()
serverMain = do
  LBS.writeFile "openapi.json" $ LBS.take 10000 apiStr
  putStrLn "wrote file"
  env <- newEnv
  run 8000 $
    gzip def $
      serve (Proxy :: Proxy API) $
        hoistServer (Proxy @API) toHandler $
          hoistServer (Proxy @API) (runWithEnv env) handler
