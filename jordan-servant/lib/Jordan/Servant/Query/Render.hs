{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Jordan.Servant.Query.Render where

import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Builder.Scientific
import Data.ByteString.Lazy (toStrict)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Void
import Jordan.ToJSON.Builder
import Jordan.ToJSON.Class
import Network.HTTP.Types.URI

newtype QueryRender a = QueryRender {runQueryRender :: a -> Query}
  deriving (Semigroup, Monoid) via (a -> Query)

instance Contravariant QueryRender where
  contramap f (QueryRender a) = QueryRender $ a . f

instance Divisible QueryRender where
  conquer = QueryRender mempty
  divide div (QueryRender renderB) (QueryRender renderC) = QueryRender $ \a ->
    let (b, c) = div a in renderB b <> renderC c

instance Selectable QueryRender where
  giveUp f = QueryRender $ absurd . f
  select sel renderL renderR =
    QueryRender $
      either
        (runQueryRender renderL)
        (runQueryRender renderR)
        . sel

escapeBracketComponent :: T.Text -> BS.ByteString
escapeBracketComponent text = endsEscaped
  where
    encoded = encodeUtf8 text
    firstEscaped = case BS.stripPrefix "[" encoded of
      Nothing -> encoded
      Just bs -> "\\[" <> bs
    endsEscaped = BS.intercalate "]]" $ BS.split 93 firstEscaped

escapeRawComponent :: T.Text -> BS.ByteString
escapeRawComponent text = startingEscaped
  where
    encoded = encodeUtf8 text
    startingEscaped = BS.intercalate "[[" $ BS.split 91 encoded

addBracked :: T.Text -> BS.ByteString -> BS.ByteString
addBracked key v =
  "[" <> escapeBracketComponent key <> "]" <> v

addArray :: BS.ByteString -> BS.ByteString
addArray v =
  "[]" <> v

instance JSONObjectSerializer QueryRender where
  serializeFieldWith name = \(QueryRender f) -> QueryRender $ \other ->
    map (first $ addBracked name) $ f other
  serializeJust name qr = QueryRender $ \case
    Nothing -> []
    Just a -> map (first $ addBracked name) $ runQueryRender qr a

instance JSONTupleSerializer QueryRender where
  serializeItemWith = \(QueryRender f) -> QueryRender $ \other ->
    map (first addArray) $ f other

instance JSONSerializer QueryRender where
  serializeObject = \x -> x
  serializeTuple = \x -> x
  serializeTextConstant t = QueryRender $ const [(mempty, Just (encodeUtf8 t))]
  serializeArray =
    QueryRender $
      foldMap $ fmap (first addArray) . runQueryRender toJSON
  serializeNumber = QueryRender $ \num ->
    [(mempty, Just $ toStrict $ toLazyByteString $ scientificBuilder num)]
  serializeNull = QueryRender $ const [(mempty, Nothing)]
  serializeText = QueryRender $ \t ->
    [(mempty, Just (encodeUtf8 t))]
  serializeBool = QueryRender $ \b ->
    pure
      (mempty, if b then Just "t" else Just "f")
  serializeDictionary = \(QueryRender renderItem) -> QueryRender $
    foldMap $ \(key, v) ->
      first (addBracked key) <$> renderItem v

renderQueryAtKeyWith :: (forall jsonSerializer. (JSONSerializer jsonSerializer) => jsonSerializer a) -> T.Text -> a -> Query
renderQueryAtKeyWith (QueryRender k) key =
  fmap (first (escapeRawComponent key <>)) . k

renderQueryAtKey :: (ToJSON a) => T.Text -> a -> Query
renderQueryAtKey = renderQueryAtKeyWith toJSON
