{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Jordan.Benchmark.OnlyWanted
    where

import Control.DeepSeq
import Data.Aeson ((.:))
import qualified Data.Aeson as A
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Jordan as J

newtype OnlyWanted
  = OnlyWanted { wanted :: Text }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass NFData

instance A.FromJSON OnlyWanted where
  parseJSON = A.withObject "onlyWanted" $ \o -> OnlyWanted <$> (o .: "wanted")

instance J.FromJSON OnlyWanted where
  fromJSON = J.parseObject "onlyWanted" $
    OnlyWanted <$> J.parseField "wanted"
