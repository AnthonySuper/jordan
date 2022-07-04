{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Jordan.Servant.Example where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.Maybe (mapMaybe)
import Data.Text (Text, isInfixOf)
import qualified Data.Text as T
import GHC.Generics
import Jordan
import Jordan.Servant
import Jordan.Servant.Example.ServerM
import Jordan.Servant.Server
import Jordan.Types.Internal.AccumE
import Jordan.Types.JSONError
import Optics
import Servant.API
import Servant.Server
import Servant.Server.UVerb

data CreatePerson = CreatePerson
  { firstName :: Text,
    lastName :: Text
  }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CreatePersonErrors = CreatePersonErrors
  { firstNameErrors :: [Text],
    lastNameErrors :: [Text]
  }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup CreatePersonErrors where
  lhs <> rhs =
    CreatePersonErrors
      { firstNameErrors = firstNameErrors lhs <> firstNameErrors rhs,
        lastNameErrors = lastNameErrors lhs <> lastNameErrors rhs
      }

instance Monoid CreatePersonErrors where
  mempty = CreatePersonErrors mempty mempty

data QueryFilter = MkQueryFilter
  { firstNameIncludes :: Maybe Text,
    lastNameIncludes :: Maybe Text
  }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

parseOptional :: (JSONObjectParser f, FromJSON a) => Text -> f (Maybe a)
parseOptional f = parseFieldWithDefault f (Just <$> fromJSON) Nothing


type Create =
  Summary "Create a person"
    :> ReportingRequestBody CreatePerson
    :> UVerb 'POST '[JSON] [WithStatus 200 (ViaJordan Person), WithStatus 400 (ViaJordan CreatePersonErrors)]

toFilter :: Maybe QueryFilter -> [Person] -> [Person]
toFilter Nothing = id
toFilter (Just MkQueryFilter {..}) =
  mapMaybe $
    maybe Just (toFilter personFirstName) firstNameIncludes
      >=> maybe Just (toFilter personLastName) lastNameIncludes
  where
    toFilter get q p = if q `isInfixOf` get p then pure p else Nothing

type List = Summary "List people" :> OptionalJordanQuery "filter" QueryFilter :> Get '[JSON] (ViaJordan [Person])

type API =
  "people" :> (Create :<|> List)

ensureNonEmpty t l
  | T.length t == 0 = AccumEL (mempty @CreatePersonErrors & l %~ (<> ["cannot be empty"]))
  | otherwise = pure t

handleCreate :: ServerT Create ServerM
handleCreate CreatePerson {..} =
  let mp = MkPerson <$> ensureNonEmpty firstName #firstNameErrors <*> ensureNonEmpty lastName #lastNameErrors
   in runToUnion $ do
        person <- lowerEither $ first (WithStatus @400 . ViaJordan) $ getAccumE mp
        lift $ addPerson person
        pure $ WithStatus @200 $ ViaJordan person

handleList :: ServerT List ServerM
handleList qf = ViaJordan . toFilter qf <$> readPeople

handler :: ServerT API ServerM
handler = handleCreate :<|> handleList
