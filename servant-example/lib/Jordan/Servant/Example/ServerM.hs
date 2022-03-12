{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Jordan.Servant.Example.ServerM where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.OpenApi.Schema (ToSchema (..))
import Data.SOP (I (I))
import Data.Text (Text)
import GHC.Generics
import Jordan
import Jordan.OpenAPI
import Servant.API.UVerb

data Person = MkPerson
  { firstName :: Text,
    lastName :: Text
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving (ToSchema) via (JordanToJSONSchema Person)

personFirstName, personLastName :: Person -> Text
personFirstName = firstName
personLastName = lastName

newtype ServerEnv = ServerEnv
  { people :: TVar [Person]
  }

newtype ServerM a = ServerM {runServerM :: ServerEnv -> IO a}
  deriving
    (Functor, Applicative, Monad, MonadReader ServerEnv, MonadIO)
    via (ReaderT ServerEnv IO)

newtype UnionError errs m a = UnionErrs {runUnionErrors :: m (Either (Union errs) a)}
  deriving
    (Functor, Applicative, Monad, MonadIO)
    via (ExceptT (Union errs) m)
  deriving (MonadTrans) via (ExceptT (Union errs))
  deriving (MonadReader r) via (ExceptT (Union errs) m)

throwErr = UnionErrs . pure . Left . inject . I

catchErr :: forall err errors m a. (IsMember err errors, Monad m) => UnionError errors m a -> (err -> UnionError errors m a) -> UnionError errors m a
m `catchErr` c = UnionErrs $ do
  r <- runUnionErrors m
  case r of
    Left ns -> case matchUnion @err ns of
      Just a -> runUnionErrors $ c a
      Nothing -> pure $ Left ns
    Right a -> pure (Right a)

lowerEither e = case e of
  Left err -> throwErr err
  Right a -> pure a

runToUnion :: (IsMember a errors, Monad m) => UnionError errors m a -> m (Union errors)
runToUnion act = do
  r <- runUnionErrors act
  case r of
    Left ns -> pure ns
    Right a -> pure $ inject (I a)

addPerson :: Person -> ServerM Person
addPerson p = do
  r <- people <$> ask
  liftIO $ atomically $ modifyTVar' r (p :)
  pure p

readPeople :: ServerM [Person]
readPeople = do
  var <- people <$> ask
  liftIO $ readTVarIO var

newEnv = ServerEnv <$> newTVarIO mempty

runWithEnv :: ServerEnv -> ServerM a -> IO a
runWithEnv env i = runServerM i env
