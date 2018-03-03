{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TemplateHaskell #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
module Data.OpenAPI.Internal.AesonUtils (
    -- * Generic functions
    AesonDefaultValue(..),
    sopOpenAPIGenericToJSON,
#if MIN_VERSION_aeson(0,10,0)
    sopOpenAPIGenericToEncoding,
#endif
    sopOpenAPIGenericToJSONWithOpts,
    sopOpenAPIGenericParseJSON,
    -- * Options
    HasOpenAPIAesonOptions(..),
    OpenAPIAesonOptions,
    mkOpenAPIAesonOptions,
    saoPrefix,
    saoAdditionalPairs,
    saoSubObject,
    ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative ((<|>))
import Control.Lens     (makeLenses, (^.))
import Control.Monad    (unless)
import Data.Aeson       (ToJSON(..), FromJSON(..), Value(..), Object, object, (.:), (.:?), (.!=), withObject)
import Data.Aeson.Types (Parser, Pair)
import Data.Char        (toLower, isUpper)
import Data.Foldable    (traverse_)
import Data.Text        (Text)

import Generics.SOP

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.HashMap.Strict.InsOrd as InsOrd

#if MIN_VERSION_aeson(0,10,0)
import Data.Aeson (Encoding, pairs, (.=), Series)
import Data.Monoid ((<>))
#endif

-------------------------------------------------------------------------------
-- OpenAPIAesonOptions
-------------------------------------------------------------------------------

data OpenAPIAesonOptions = OpenAPIAesonOptions
    { _saoPrefix          :: String
    , _saoAdditionalPairs :: [(Text, Value)]
    , _saoSubObject       :: Maybe String
    }

mkOpenAPIAesonOptions
    :: String  -- ^ prefix
    -> OpenAPIAesonOptions
mkOpenAPIAesonOptions pfx = OpenAPIAesonOptions pfx [] Nothing

makeLenses ''OpenAPIAesonOptions

class (Generic a, All2 AesonDefaultValue (Code a)) => HasOpenAPIAesonOptions a where
    openapiAesonOptions :: proxy a -> OpenAPIAesonOptions

    -- So far we use only default definitions
    aesonDefaults :: proxy a -> POP Maybe (Code a)
    aesonDefaults _ = hcpure (Proxy :: Proxy AesonDefaultValue) defaultValue

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

class AesonDefaultValue a where
    defaultValue :: Maybe a
    defaultValue = Nothing

instance AesonDefaultValue Text where defaultValue = Nothing
instance AesonDefaultValue (Maybe a) where defaultValue = Just Nothing
instance AesonDefaultValue [a] where defaultValue = Just []
instance AesonDefaultValue (Set.Set a) where defaultValue = Just Set.empty
instance AesonDefaultValue (InsOrd.InsOrdHashMap k v) where defaultValue = Just InsOrd.empty

-------------------------------------------------------------------------------
-- ToJSON
-------------------------------------------------------------------------------

-- | Generic serialisation for openapi records.
--
-- Features
--
-- * omits nulls, empty objects and empty arrays (configurable)
-- * possible to add fields
-- * possible to merge sub-object
sopOpenAPIGenericToJSON
    :: forall a xs.
        ( HasDatatypeInfo a
        , HasOpenAPIAesonOptions a
        , All2 ToJSON (Code a)
        , All2 Eq (Code a)
        , Code a ~ '[xs]
        )
    => a
    -> Value
sopOpenAPIGenericToJSON x =
    let ps = sopOpenAPIGenericToJSON' opts (from x) (datatypeInfo proxy) (aesonDefaults proxy)
    in object (opts ^. saoAdditionalPairs ++ ps)
  where
    proxy = Proxy :: Proxy a
    opts  = openapiAesonOptions proxy

-- | *TODO:* This is only used by ToJSON (ParamSchema OpenAPIKindSchema)
--
-- Also uses default `aesonDefaults`
sopOpenAPIGenericToJSONWithOpts
    :: forall a xs.
        ( Generic a
        , All2 AesonDefaultValue (Code a)
        , HasDatatypeInfo a
        , All2 ToJSON (Code a)
        , All2 Eq (Code a)
        , Code a ~ '[xs]
        )
    => OpenAPIAesonOptions
    -> a
    -> Value
sopOpenAPIGenericToJSONWithOpts opts x =
    let ps = sopOpenAPIGenericToJSON' opts (from x) (datatypeInfo proxy) defs
    in object (opts ^. saoAdditionalPairs ++ ps)
  where
    proxy = Proxy :: Proxy a
    defs = hcpure (Proxy :: Proxy AesonDefaultValue) defaultValue

sopOpenAPIGenericToJSON'
    :: (All2 ToJSON '[xs], All2 Eq '[xs])
    => OpenAPIAesonOptions
    -> SOP I '[xs]
    -> DatatypeInfo '[xs]
    -> POP Maybe '[xs]
    -> [Pair]
sopOpenAPIGenericToJSON' opts (SOP (Z fields)) (ADT _ _ (Record _ fieldsInfo :* Nil)) (POP (defs :* Nil)) =
    sopOpenAPIGenericToJSON'' opts fields fieldsInfo defs
sopOpenAPIGenericToJSON' _ _ _ _ = error "sopOpenAPIGenericToJSON: unsupported type"

sopOpenAPIGenericToJSON''
    :: (All ToJSON xs, All Eq xs)
    => OpenAPIAesonOptions
    -> NP I xs
    -> NP FieldInfo xs
    -> NP Maybe xs
    -> [Pair]
sopOpenAPIGenericToJSON'' (OpenAPIAesonOptions prefix _ sub) = go
  where
    go :: (All ToJSON ys, All Eq ys) => NP I ys -> NP FieldInfo ys -> NP Maybe ys -> [Pair]
    go  Nil Nil Nil = []
    go (I x :* xs) (FieldInfo name :* names) (def :* defs)
        | Just name' == sub = case json of
              Object m -> HM.toList m ++ rest
              Null     -> rest
              _        -> error $ "sopOpenAPIGenericToJSON: subjson is not an object: " ++ show json
        -- If default value: omit it.
        | Just x == def =
            rest
        | otherwise =
            (T.pack name', json) : rest
      where
        json  = toJSON x
        name' = fieldNameModifier name
        rest  = go xs names defs
#if __GLASGOW_HASKELL__ < 800
    go _ _ _ = error "not empty"
#endif

    fieldNameModifier = modifier . drop 1
    modifier = lowerFirstUppers . drop (length prefix)
    lowerFirstUppers s = map toLower x ++ y
      where (x, y) = span isUpper s

-------------------------------------------------------------------------------
-- FromJSON
-------------------------------------------------------------------------------

sopOpenAPIGenericParseJSON
    :: forall a xs.
        ( HasDatatypeInfo a
        , HasOpenAPIAesonOptions a
        , All2 FromJSON (Code a)
        , All2 Eq (Code a)
        , Code a ~ '[xs]
        )
    => Value
    -> Parser a
sopOpenAPIGenericParseJSON = withObject "OpenAPI Record Object" $ \obj ->
    let ps = sopOpenAPIGenericParseJSON' opts obj (datatypeInfo proxy) (aesonDefaults proxy)
    in do
        traverse_ (parseAdditionalField obj) (opts ^. saoAdditionalPairs)
        to <$> ps
  where
    proxy = Proxy :: Proxy a
    opts  = openapiAesonOptions proxy

    parseAdditionalField :: Object -> (Text, Value) -> Parser ()
    parseAdditionalField obj (k, v) = do
        v' <- obj .: k
        unless (v == v') $ fail $
            "Additonal field don't match for key " ++ T.unpack k
            ++ ": " ++ show v
            ++ " /= " ++ show v'

sopOpenAPIGenericParseJSON'
    :: (All2 FromJSON '[xs], All2 Eq '[xs])
    => OpenAPIAesonOptions
    -> Object
    -> DatatypeInfo '[xs]
    -> POP Maybe '[xs]
    -> Parser (SOP I '[xs])
sopOpenAPIGenericParseJSON' opts obj (ADT _ _ (Record _ fieldsInfo :* Nil)) (POP (defs :* Nil)) =
    SOP . Z <$> sopOpenAPIGenericParseJSON'' opts obj fieldsInfo defs
sopOpenAPIGenericParseJSON' _ _ _ _ = error "sopOpenAPIGenericParseJSON: unsupported type"

sopOpenAPIGenericParseJSON''
    :: (All FromJSON xs, All Eq xs)
    => OpenAPIAesonOptions
    -> Object
    -> NP FieldInfo xs
    -> NP Maybe xs
    -> Parser (NP I xs)
sopOpenAPIGenericParseJSON'' (OpenAPIAesonOptions prefix _ sub) obj = go
  where
    go :: (All FromJSON ys, All Eq ys) => NP FieldInfo ys -> NP Maybe ys -> Parser (NP I ys)
    go  Nil Nil = pure Nil
    go (FieldInfo name :* names) (def :* defs)
        | Just name' == sub =
            -- Note: we might strip fields of outer structure.
            cons <$> (withDef $ parseJSON $ Object obj) <*> rest
        | otherwise = case def of
            Just def' -> cons <$> obj .:? T.pack name' .!= def' <*> rest
            Nothing  ->  cons <$> obj .: T.pack name' <*> rest
      where
        cons h t = I h :* t
        name' = fieldNameModifier name
        rest  = go names defs

        withDef = case def of
            Just def' -> (<|> pure def')
            Nothing   -> id
#if __GLASGOW_HASKELL__ < 800
    go _ _ = error "not empty"
#endif

    fieldNameModifier = modifier . drop 1
    modifier = lowerFirstUppers . drop (length prefix)
    lowerFirstUppers s = map toLower x ++ y
      where (x, y) = span isUpper s

-------------------------------------------------------------------------------
-- ToEncoding
-------------------------------------------------------------------------------

#if MIN_VERSION_aeson(0,10,0)

sopOpenAPIGenericToEncoding
    :: forall a xs.
        ( HasDatatypeInfo a
        , HasOpenAPIAesonOptions a
        , All2 ToJSON (Code a)
        , All2 Eq (Code a)
        , Code a ~ '[xs]
        )
    => a
    -> Encoding
sopOpenAPIGenericToEncoding x =
    let ps = sopOpenAPIGenericToEncoding' opts (from x) (datatypeInfo proxy) (aesonDefaults proxy)
    in pairs (pairsToSeries (opts ^. saoAdditionalPairs) <> ps)
  where
    proxy = Proxy :: Proxy a
    opts  = openapiAesonOptions proxy

pairsToSeries :: [Pair] -> Series
pairsToSeries = foldMap (\(k, v) -> (k .= v))

sopOpenAPIGenericToEncoding'
    :: (All2 ToJSON '[xs], All2 Eq '[xs])
    => OpenAPIAesonOptions
    -> SOP I '[xs]
    -> DatatypeInfo '[xs]
    -> POP Maybe '[xs]
    -> Series
sopOpenAPIGenericToEncoding' opts (SOP (Z fields)) (ADT _ _ (Record _ fieldsInfo :* Nil)) (POP (defs :* Nil)) =
    sopOpenAPIGenericToEncoding'' opts fields fieldsInfo defs
sopOpenAPIGenericToEncoding' _ _ _ _ = error "sopOpenAPIGenericToEncoding: unsupported type"

sopOpenAPIGenericToEncoding''
    :: (All ToJSON xs, All Eq xs)
    => OpenAPIAesonOptions
    -> NP I xs
    -> NP FieldInfo xs
    -> NP Maybe xs
    -> Series
sopOpenAPIGenericToEncoding'' (OpenAPIAesonOptions prefix _ sub) = go
  where
    go :: (All ToJSON ys, All Eq ys) => NP I ys -> NP FieldInfo ys -> NP Maybe ys -> Series
    go  Nil Nil Nil = mempty
    go (I x :* xs) (FieldInfo name :* names) (def :* defs)
        | Just name' == sub = case toJSON x of
              Object m -> pairsToSeries (HM.toList m) <> rest
              Null     -> rest
              _        -> error $ "sopOpenAPIGenericToJSON: subjson is not an object: " ++ show (toJSON x)
        -- If default value: omit it.
        | Just x == def =
            rest
        | otherwise =
            (T.pack name' .= x) <> rest
      where
        name' = fieldNameModifier name
        rest  = go xs names defs
#if __GLASGOW_HASKELL__ < 800
    go _ _ _ = error "not empty"
#endif

    fieldNameModifier = modifier . drop 1
    modifier = lowerFirstUppers . drop (length prefix)
    lowerFirstUppers s = map toLower x ++ y
      where (x, y) = span isUpper s

#endif
