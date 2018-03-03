{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 800
-- Few generics related redundant constraints
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- For TypeErrors
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
#endif
#include "overlapping-compat.h"
module Data.OpenAPI.Internal.Schema where

import Prelude ()
import Prelude.Compat

import Control.Lens
import Data.Data.Lens (template)

import Control.Monad
import Control.Monad.Writer
import Data.Aeson (ToJSON (..), ToJSONKey (..), ToJSONKeyFunction (..), Value (..))
import Data.Char
import Data.Data (Data)
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           "unordered-containers" Data.HashSet (HashSet)
import qualified "unordered-containers" Data.HashSet as HashSet
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Int
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Proxy
import Data.Scientific (Scientific)
import Data.Fixed (Fixed, HasResolution, Pico)
import Data.Set (Set)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Numeric.Natural
import Data.Word
import GHC.Generics
import qualified Data.UUID.Types as UUID

import Data.OpenAPI.Declare
import Data.OpenAPI.Internal
import Data.OpenAPI.Internal.ParamSchema (ToParamSchema(..))
import Data.OpenAPI.Lens hiding (name, schema)
import qualified Data.OpenAPI.Lens as OpenAPI
import Data.OpenAPI.SchemaOptions
import Data.OpenAPI.Internal.TypeShape

#if __GLASGOW_HASKELL__ < 800
#else
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import GHC.TypeLits (TypeError, ErrorMessage(..))
#endif

unnamed :: Schema -> NamedSchema
unnamed schema = NamedSchema Nothing schema

named :: T.Text -> Schema -> NamedSchema
named name schema = NamedSchema (Just name) schema

plain :: Schema -> Declare (Definitions Schema) NamedSchema
plain = pure . unnamed

unname :: NamedSchema -> NamedSchema
unname (NamedSchema _ schema) = unnamed schema

rename :: Maybe T.Text -> NamedSchema -> NamedSchema
rename name (NamedSchema _ schema) = NamedSchema name schema

-- | Convert a type into @'Schema'@.
--
-- An example type and instance:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}   -- allows to write 'T.Text' literals
-- {-\# LANGUAGE OverloadedLists \#-}     -- allows to write 'Map' and 'HashMap' as lists
--
-- import Control.Lens
-- import Data.Proxy
-- import Data.OpenAPI
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance ToSchema Coord where
--   declareNamedSchema _ = do
--     doubleSchema <- declareSchemaRef (Proxy :: Proxy Double)
--     return $ NamedSchema (Just \"Coord\") $ mempty
--       & type_ .~ OpenAPIObject
--       & properties .~
--           [ (\"x\", doubleSchema)
--           , (\"y\", doubleSchema)
--           ]
--       & required .~ [ \"x\", \"y\" ]
-- @
--
-- Instead of manually writing your @'ToSchema'@ instance you can
-- use a default generic implementation of @'declareNamedSchema'@.
--
-- To do that, simply add @deriving 'Generic'@ clause to your datatype
-- and declare a @'ToSchema'@ instance for your datatype without
-- giving definition for @'declareNamedSchema'@.
--
-- For instance, the previous example can be simplified into this:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import GHC.Generics (Generic)
--
-- data Coord = Coord { x :: Double, y :: Double } deriving Generic
--
-- instance ToSchema Coord
-- @
class ToSchema a where
  -- | Convert a type into an optionally named schema
  -- together with all used definitions.
  -- Note that the schema itself is included in definitions
  -- only if it is recursive (and thus needs its definition in scope).
  declareNamedSchema :: proxy a -> Declare (Definitions Schema) NamedSchema
  default declareNamedSchema :: (Generic a, GToSchema (Rep a), TypeHasSimpleShape a "genericDeclareNamedSchemaUnrestricted") =>
    proxy a -> Declare (Definitions Schema) NamedSchema
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

-- | Convert a type into a schema and declare all used schema definitions.
declareSchema :: ToSchema a => proxy a -> Declare (Definitions Schema) Schema
declareSchema = fmap _namedSchemaSchema . declareNamedSchema

-- | Convert a type into an optionally named schema.
--
-- >>> toNamedSchema (Proxy :: Proxy String) ^. name
-- Nothing
-- >>> encode (toNamedSchema (Proxy :: Proxy String) ^. schema)
-- "{\"type\":\"string\"}"
--
-- >>> toNamedSchema (Proxy :: Proxy Day) ^. name
-- Just "Day"
-- >>> encode (toNamedSchema (Proxy :: Proxy Day) ^. schema)
-- "{\"example\":\"2016-07-22\",\"format\":\"date\",\"type\":\"string\"}"
toNamedSchema :: ToSchema a => proxy a -> NamedSchema
toNamedSchema = undeclare . declareNamedSchema

-- | Get type's schema name according to its @'ToSchema'@ instance.
--
-- >>> schemaName (Proxy :: Proxy Int)
-- Nothing
--
-- >>> schemaName (Proxy :: Proxy UTCTime)
-- Just "UTCTime"
schemaName :: ToSchema a => proxy a -> Maybe T.Text
schemaName = _namedSchemaName . toNamedSchema

-- | Convert a type into a schema.
--
-- >>> encode $ toSchema (Proxy :: Proxy Int8)
-- "{\"maximum\":127,\"minimum\":-128,\"type\":\"integer\"}"
--
-- >>> encode $ toSchema (Proxy :: Proxy [Day])
-- "{\"items\":{\"$ref\":\"#/definitions/Day\"},\"type\":\"array\"}"
toSchema :: ToSchema a => proxy a -> Schema
toSchema = _namedSchemaSchema . toNamedSchema

-- | Convert a type into a referenced schema if possible.
-- Only named schemas can be referenced, nameless schemas are inlined.
--
-- >>> encode $ toSchemaRef (Proxy :: Proxy Integer)
-- "{\"type\":\"integer\"}"
--
-- >>> encode $ toSchemaRef (Proxy :: Proxy Day)
-- "{\"$ref\":\"#/definitions/Day\"}"
toSchemaRef :: ToSchema a => proxy a -> Referenced Schema
toSchemaRef = undeclare . declareSchemaRef

-- | Convert a type into a referenced schema if possible
-- and declare all used schema definitions.
-- Only named schemas can be referenced, nameless schemas are inlined.
--
-- Schema definitions are typically declared for every referenced schema.
-- If @'declareSchemaRef'@ returns a reference, a corresponding schema
-- will be declared (regardless of whether it is recusive or not).
declareSchemaRef :: ToSchema a => proxy a -> Declare (Definitions Schema) (Referenced Schema)
declareSchemaRef proxy = do
  case toNamedSchema proxy of
    NamedSchema (Just name) schema -> do
      -- This check is very important as it allows generically
      -- derive used definitions for recursive schemas.
      -- Lazy Declare monad allows toNamedSchema to ignore
      -- any declarations (which would otherwise loop) and
      -- retrieve the schema and its name to check if we
      -- have already declared it.
      -- If we have, we don't need to declare anything for
      -- this schema this time and thus simply return the reference.
      known <- looks (InsOrdHashMap.member name)
      when (not known) $ do
        declare [(name, schema)]
        void $ declareNamedSchema proxy
      return $ Ref (Reference name)
    _ -> Inline <$> declareSchema proxy

-- | Inline any referenced schema if its name satisfies given predicate.
--
-- /NOTE:/ if a referenced schema is not found in definitions the predicate is ignored
-- and schema stays referenced.
--
-- __WARNING:__ @'inlineSchemasWhen'@ will produce infinite schemas
-- when inlining recursive schemas.
inlineSchemasWhen :: Data s => (T.Text -> Bool) -> (Definitions Schema) -> s -> s
inlineSchemasWhen p defs = template %~ deref
  where
    deref r@(Ref (Reference name))
      | p name =
          case InsOrdHashMap.lookup name defs of
            Just schema -> Inline (inlineSchemasWhen p defs schema)
            Nothing -> r
      | otherwise = r
    deref (Inline schema) = Inline (inlineSchemasWhen p defs schema)

-- | Inline any referenced schema if its name is in the given list.
--
-- /NOTE:/ if a referenced schema is not found in definitions
-- it stays referenced even if it appears in the list of names.
--
-- __WARNING:__ @'inlineSchemas'@ will produce infinite schemas
-- when inlining recursive schemas.
inlineSchemas :: Data s => [T.Text] -> (Definitions Schema) -> s -> s
inlineSchemas names = inlineSchemasWhen (`elem` names)

-- | Inline all schema references for which the definition
-- can be found in @'Definitions'@.
--
-- __WARNING:__ @'inlineAllSchemas'@ will produce infinite schemas
-- when inlining recursive schemas.
inlineAllSchemas :: Data s => (Definitions Schema) -> s -> s
inlineAllSchemas = inlineSchemasWhen (const True)

-- | Convert a type into a schema without references.
--
-- >>> encode $ toInlinedSchema (Proxy :: Proxy [Day])
-- "{\"items\":{\"example\":\"2016-07-22\",\"format\":\"date\",\"type\":\"string\"},\"type\":\"array\"}"
--
-- __WARNING:__ @'toInlinedSchema'@ will produce infinite schema
-- when inlining recursive schemas.
toInlinedSchema :: ToSchema a => proxy a -> Schema
toInlinedSchema proxy = inlineAllSchemas defs schema
  where
    (defs, schema) = runDeclare (declareSchema proxy) mempty

-- | Inline all /non-recursive/ schemas for which the definition
-- can be found in @'Definitions'@.
inlineNonRecursiveSchemas :: Data s => (Definitions Schema) -> s -> s
inlineNonRecursiveSchemas defs = inlineSchemasWhen nonRecursive defs
  where
    nonRecursive name =
      case InsOrdHashMap.lookup name defs of
        Just schema -> name `notElem` execDeclare (usedNames schema) mempty
        Nothing     -> False

    usedNames schema = traverse_ schemaRefNames (schema ^.. template)

    schemaRefNames :: Referenced Schema -> Declare [T.Text] ()
    schemaRefNames ref = case ref of
      Ref (Reference name) -> do
        seen <- looks (name `elem`)
        when (not seen) $ do
          declare [name]
          traverse_ usedNames (InsOrdHashMap.lookup name defs)
      Inline subschema -> usedNames subschema

-- | Default schema for binary data (any sequence of octets).
binarySchema :: Schema
binarySchema = mempty
  & type_ .~ OpenAPIString
  & format ?~ "binary"

-- | Default schema for binary data (base64 encoded).
byteSchema :: Schema
byteSchema = mempty
  & type_ .~ OpenAPIString
  & format ?~ "byte"

-- | Default schema for password string.
-- @"password"@ format is used to hint UIs the input needs to be obscured.
passwordSchema :: Schema
passwordSchema = mempty
  & type_ .~ OpenAPIString
  & format ?~ "password"

-- | Make an unrestrictive sketch of a @'Schema'@ based on a @'ToJSON'@ instance.
-- Produced schema can be used for further refinement.
--
-- >>> encode $ sketchSchema "hello"
-- "{\"example\":\"hello\",\"type\":\"string\"}"
--
-- >>> encode $ sketchSchema (1, 2, 3)
-- "{\"example\":[1,2,3],\"items\":{\"type\":\"number\"},\"type\":\"array\"}"
--
-- >>> encode $ sketchSchema ("Jack", 25)
-- "{\"example\":[\"Jack\",25],\"items\":[{\"type\":\"string\"},{\"type\":\"number\"}],\"type\":\"array\"}"
--
-- >>> data Person = Person { name :: String, age :: Int } deriving (Generic)
-- >>> instance ToJSON Person
-- >>> encode $ sketchSchema (Person "Jack" 25)
-- "{\"required\":[\"age\",\"name\"],\"properties\":{\"age\":{\"type\":\"number\"},\"name\":{\"type\":\"string\"}},\"example\":{\"age\":25,\"name\":\"Jack\"},\"type\":\"object\"}"
sketchSchema :: ToJSON a => a -> Schema
sketchSchema = sketch . toJSON
  where
    sketch Null = go Null
    sketch js@(Bool _) = go js
    sketch js = go js & example ?~ js

    go Null       = mempty & type_ .~ OpenAPINull
    go (Bool _)   = mempty & type_ .~ OpenAPIBoolean
    go (String _) = mempty & type_   .~ OpenAPIString
    go (Number _) = mempty & type_ .~ OpenAPINumber
    go (Array xs) = mempty
      & type_   .~ OpenAPIArray
      & items ?~ case ischema of
          Just s -> OpenAPIItemsObject (Inline s)
          _      -> OpenAPIItemsArray (map Inline ys)
      where
        ys = map go (V.toList xs)
        allSame = and ((zipWith (==)) ys (tail ys))

        ischema = case ys of
          (z:_) | allSame -> Just z
          _               -> Nothing
    go (Object o) = mempty
      & type_         .~ OpenAPIObject
      & required      .~ HashMap.keys o
      & properties    .~ fmap (Inline . go) (InsOrdHashMap.fromHashMap o)

-- | Make a restrictive sketch of a @'Schema'@ based on a @'ToJSON'@ instance.
-- Produced schema uses as much constraints as possible.
--
-- >>> encode $ sketchStrictSchema "hello"
-- "{\"maxLength\":5,\"pattern\":\"hello\",\"minLength\":5,\"type\":\"string\",\"enum\":[\"hello\"]}"
--
-- >>> encode $ sketchStrictSchema (1, 2, 3)
-- "{\"minItems\":3,\"uniqueItems\":true,\"items\":[{\"maximum\":1,\"minimum\":1,\"multipleOf\":1,\"type\":\"number\",\"enum\":[1]},{\"maximum\":2,\"minimum\":2,\"multipleOf\":2,\"type\":\"number\",\"enum\":[2]},{\"maximum\":3,\"minimum\":3,\"multipleOf\":3,\"type\":\"number\",\"enum\":[3]}],\"maxItems\":3,\"type\":\"array\",\"enum\":[[1,2,3]]}"
--
-- >>> encode $ sketchStrictSchema ("Jack", 25)
-- "{\"minItems\":2,\"uniqueItems\":true,\"items\":[{\"maxLength\":4,\"pattern\":\"Jack\",\"minLength\":4,\"type\":\"string\",\"enum\":[\"Jack\"]},{\"maximum\":25,\"minimum\":25,\"multipleOf\":25,\"type\":\"number\",\"enum\":[25]}],\"maxItems\":2,\"type\":\"array\",\"enum\":[[\"Jack\",25]]}"
--
-- >>> data Person = Person { name :: String, age :: Int } deriving (Generic)
-- >>> instance ToJSON Person
-- >>> encode $ sketchStrictSchema (Person "Jack" 25)
-- "{\"required\":[\"age\",\"name\"],\"properties\":{\"age\":{\"maximum\":25,\"minimum\":25,\"multipleOf\":25,\"type\":\"number\",\"enum\":[25]},\"name\":{\"maxLength\":4,\"pattern\":\"Jack\",\"minLength\":4,\"type\":\"string\",\"enum\":[\"Jack\"]}},\"maxProperties\":2,\"minProperties\":2,\"type\":\"object\",\"enum\":[{\"age\":25,\"name\":\"Jack\"}]}"
sketchStrictSchema :: ToJSON a => a -> Schema
sketchStrictSchema = go . toJSON
  where
    go Null       = mempty & type_ .~ OpenAPINull
    go js@(Bool _) = mempty
      & type_ .~ OpenAPIBoolean
      & enum_ ?~ [js]
    go js@(String s) = mempty
      & type_ .~ OpenAPIString
      & maxLength ?~ fromIntegral (T.length s)
      & minLength ?~ fromIntegral (T.length s)
      & pattern   ?~ s
      & enum_     ?~ [js]
    go js@(Number n) = mempty
      & type_       .~ OpenAPINumber
      & maximum_    ?~ n
      & minimum_    ?~ n
      & multipleOf  ?~ n
      & enum_       ?~ [js]
    go js@(Array xs) = mempty
      & type_       .~ OpenAPIArray
      & maxItems    ?~ fromIntegral sz
      & minItems    ?~ fromIntegral sz
      & items       ?~ OpenAPIItemsArray (map (Inline . go) (V.toList xs))
      & uniqueItems ?~ allUnique
      & enum_       ?~ [js]
      where
        sz = length xs
        allUnique = sz == HashSet.size (HashSet.fromList (V.toList xs))
    go js@(Object o) = mempty
      & type_         .~ OpenAPIObject
      & required      .~ names
      & properties    .~ fmap (Inline . go) (InsOrdHashMap.fromHashMap o)
      & maxProperties ?~ fromIntegral (length names)
      & minProperties ?~ fromIntegral (length names)
      & enum_         ?~ [js]
      where
        names = HashMap.keys o

class GToSchema (f :: * -> *) where
  gdeclareNamedSchema :: SchemaOptions -> proxy f -> Schema -> Declare (Definitions Schema) NamedSchema

instance OVERLAPPABLE_ ToSchema a => ToSchema [a] where
  declareNamedSchema _ = do
    ref <- declareSchemaRef (Proxy :: Proxy a)
    return $ unnamed $ mempty
      & type_ .~ OpenAPIArray
      & items ?~ OpenAPIItemsObject ref

instance OVERLAPPING_ ToSchema String where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Bool    where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Integer where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Natural where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Int     where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Int8    where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Int16   where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Int32   where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Int64   where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Word    where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Word8   where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Word16  where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Word32  where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Word64  where declareNamedSchema = plain . paramSchemaToSchema

instance ToSchema Char where
  declareNamedSchema proxy = plain (paramSchemaToSchema proxy)
    & mapped.OpenAPI.schema.example ?~ toJSON '?'

instance ToSchema Scientific  where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Double      where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Float       where declareNamedSchema = plain . paramSchemaToSchema

instance HasResolution a => ToSchema (Fixed a) where declareNamedSchema = plain . paramSchemaToSchema

instance ToSchema a => ToSchema (Maybe a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy a)

instance (ToSchema a, ToSchema b) => ToSchema (Either a b)

instance ToSchema () where
  declareNamedSchema _ = pure (NamedSchema Nothing nullarySchema)

-- | For 'ToJSON' instance, see <http://hackage.haskell.org/package/uuid-aeson uuid-aeson> package.
instance ToSchema UUID.UUID where
  declareNamedSchema p = pure $ named "UUID" $ paramSchemaToSchema p
    & example ?~ toJSON (UUID.toText UUID.nil)

instance (ToSchema a, ToSchema b) => ToSchema (a, b)
instance (ToSchema a, ToSchema b, ToSchema c) => ToSchema (a, b, c)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d) => ToSchema (a, b, c, d)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e) => ToSchema (a, b, c, d, e)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e, ToSchema f) => ToSchema (a, b, c, d, e, f)
instance (ToSchema a, ToSchema b, ToSchema c, ToSchema d, ToSchema e, ToSchema f, ToSchema g) => ToSchema (a, b, c, d, e, f, g)

timeSchema :: T.Text -> Schema
timeSchema fmt = mempty
  & type_ .~ OpenAPIString
  & format ?~ fmt

-- | Format @"date"@ corresponds to @yyyy-mm-dd@ format.
instance ToSchema Day where
  declareNamedSchema _ = pure $ named "Day" $ timeSchema "date"
    & example ?~ toJSON (fromGregorian 2016 7 22)

-- |
-- >>> toSchema (Proxy :: Proxy LocalTime) ^. format
-- Just "yyyy-mm-ddThh:MM:ss"
instance ToSchema LocalTime where
  declareNamedSchema _ = pure $ named "LocalTime" $ timeSchema "yyyy-mm-ddThh:MM:ss"
    & example ?~ toJSON (LocalTime (fromGregorian 2016 7 22) (TimeOfDay 7 40 0))

-- | Format @"date"@ corresponds to @yyyy-mm-ddThh:MM:ss(Z|+hh:MM)@ format.
instance ToSchema ZonedTime where
  declareNamedSchema _ = pure $ named "ZonedTime" $ timeSchema "date-time"
    & example ?~ toJSON (ZonedTime (LocalTime (fromGregorian 2016 7 22) (TimeOfDay 7 40 0)) (hoursToTimeZone 3))

instance ToSchema NominalDiffTime where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Pico)

-- |
-- >>> toSchema (Proxy :: Proxy UTCTime) ^. format
-- Just "yyyy-mm-ddThh:MM:ssZ"
instance ToSchema UTCTime where
  declareNamedSchema _ = pure $ named "UTCTime" $ timeSchema "yyyy-mm-ddThh:MM:ssZ"
    & example ?~ toJSON (UTCTime (fromGregorian 2016 7 22) 0)

instance ToSchema T.Text where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema TL.Text where declareNamedSchema = plain . paramSchemaToSchema

#if __GLASGOW_HASKELL__ < 800
#else
type family ToSchemaByteStringError bs where
  ToSchemaByteStringError bs = TypeError
      ( Text "Impossible to have an instance " :<>: ShowType (ToSchema bs) :<>: Text "."
   :$$: Text "Please, use a newtype wrapper around " :<>: ShowType bs :<>: Text " instead."
   :$$: Text "Consider using byteSchema or binarySchema templates." )

instance ToSchemaByteStringError BS.ByteString  => ToSchema BS.ByteString  where declareNamedSchema = error "impossible"
instance ToSchemaByteStringError BSL.ByteString => ToSchema BSL.ByteString where declareNamedSchema = error "impossible"
#endif

instance ToSchema IntSet where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Set Int))

-- | NOTE: This schema does not account for the uniqueness of keys.
instance ToSchema a => ToSchema (IntMap a) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [(Int, a)])

#if MIN_VERSION_aeson(1,0,0)
instance (ToJSONKey k, ToSchema k, ToSchema v) => ToSchema (Map k v) where
  declareNamedSchema _ = case toJSONKey :: ToJSONKeyFunction k of
      ToJSONKeyText  _ _ -> declareObjectMapSchema
      ToJSONKeyValue _ _ -> declareNamedSchema (Proxy :: Proxy [(k, v)])
    where
      declareObjectMapSchema = do
        schema <- declareSchemaRef (Proxy :: Proxy v)
        return $ unnamed $ mempty
          & type_ .~ OpenAPIObject
          & additionalProperties ?~ schema

instance (ToJSONKey k, ToSchema k, ToSchema v) => ToSchema (HashMap k v) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Map k v))

#else

instance ToSchema a => ToSchema (Map String a) where
  declareNamedSchema _ = do
    schema <- declareSchemaRef (Proxy :: Proxy a)
    return $ unnamed $ mempty
      & type_ .~ OpenAPIObject
      & additionalProperties ?~ schema

instance ToSchema a => ToSchema (Map T.Text  a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (Map TL.Text a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Map String a))

instance ToSchema a => ToSchema (HashMap String  a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (HashMap T.Text  a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Map String a))
instance ToSchema a => ToSchema (HashMap TL.Text a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Map String a))

#endif

instance ToSchema a => ToSchema (V.Vector a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [a])
instance ToSchema a => ToSchema (VU.Vector a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [a])
instance ToSchema a => ToSchema (VS.Vector a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [a])
instance ToSchema a => ToSchema (VP.Vector a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy [a])

instance ToSchema a => ToSchema (Set a) where
  declareNamedSchema _ = do
    schema <- declareSchema (Proxy :: Proxy [a])
    return $ unnamed $ schema
      & uniqueItems ?~ True

instance ToSchema a => ToSchema (HashSet a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Set a))

instance ToSchema All where declareNamedSchema = plain . paramSchemaToSchema
instance ToSchema Any where declareNamedSchema = plain . paramSchemaToSchema

instance ToSchema a => ToSchema (Sum a)     where declareNamedSchema _ = unname <$> declareNamedSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Product a) where declareNamedSchema _ = unname <$> declareNamedSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (First a)   where declareNamedSchema _ = unname <$> declareNamedSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Last a)    where declareNamedSchema _ = unname <$> declareNamedSchema (Proxy :: Proxy a)
instance ToSchema a => ToSchema (Dual a)    where declareNamedSchema _ = unname <$> declareNamedSchema (Proxy :: Proxy a)

instance ToSchema a => ToSchema (Identity a) where declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy a)

-- | Default schema for @'Bounded'@, @'Integral'@ types.
--
-- >>> encode $ toSchemaBoundedIntegral (Proxy :: Proxy Int16)
-- "{\"maximum\":32767,\"minimum\":-32768,\"type\":\"integer\"}"
toSchemaBoundedIntegral :: forall a proxy. (Bounded a, Integral a) => proxy a -> Schema
toSchemaBoundedIntegral _ = mempty
  & type_ .~ OpenAPIInteger
  & minimum_ ?~ fromInteger (toInteger (minBound :: a))
  & maximum_ ?~ fromInteger (toInteger (maxBound :: a))

-- | Default generic named schema for @'Bounded'@, @'Integral'@ types.
genericToNamedSchemaBoundedIntegral :: forall a d f proxy.
  ( Bounded a, Integral a
  , Generic a, Rep a ~ D1 d f, Datatype d)
  => SchemaOptions -> proxy a -> NamedSchema
genericToNamedSchemaBoundedIntegral opts proxy
  = genericNameSchema opts proxy (toSchemaBoundedIntegral proxy)

-- | Declare a named schema for a @newtype@ wrapper.
genericDeclareNamedSchemaNewtype :: forall proxy a d c s i inner.
  (Generic a, Datatype d, Rep a ~ D1 d (C1 c (S1 s (K1 i inner))))
  => SchemaOptions                                          -- ^ How to derive the name.
  -> (Proxy inner -> Declare (Definitions Schema) Schema)   -- ^ How to create a schema for the wrapped type.
  -> proxy a
  -> Declare (Definitions Schema) NamedSchema
genericDeclareNamedSchemaNewtype opts f proxy = genericNameSchema opts proxy <$> f (Proxy :: Proxy inner)

-- | Declare 'Schema' for a mapping with 'Bounded' 'Enum' keys.
-- This makes a much more useful schema when there aren't many options for key values.
--
-- >>> data ButtonState = Neutral | Focus | Active | Hover | Disabled deriving (Show, Bounded, Enum, Generic)
-- >>> instance ToJSON ButtonState
-- >>> instance ToSchema ButtonState
-- >>> instance ToJSONKey ButtonState where toJSONKey = toJSONKeyText (T.pack . show)
-- >>> type ImageUrl = T.Text
-- >>> encode $ toSchemaBoundedEnumKeyMapping (Proxy :: Proxy (Map ButtonState ImageUrl))
-- "{\"properties\":{\"Neutral\":{\"type\":\"string\"},\"Focus\":{\"type\":\"string\"},\"Active\":{\"type\":\"string\"},\"Hover\":{\"type\":\"string\"},\"Disabled\":{\"type\":\"string\"}},\"type\":\"object\"}"
--
-- Note: this is only useful when @key@ is encoded with 'ToJSONKeyText'.
-- If it is encoded with 'ToJSONKeyValue' then a regular schema for @[(key, value)]@ is used.
declareSchemaBoundedEnumKeyMapping :: forall map key value proxy.
  (Bounded key, Enum key, ToJSONKey key, ToSchema key, ToSchema value)
  => proxy (map key value) -> Declare (Definitions Schema) Schema
declareSchemaBoundedEnumKeyMapping _ = case toJSONKey :: ToJSONKeyFunction key of
  ToJSONKeyText keyToText _ -> objectSchema keyToText
  ToJSONKeyValue _ _ -> declareSchema (Proxy :: Proxy [(key, value)])
  where
    objectSchema keyToText = do
      valueRef <- declareSchemaRef (Proxy :: Proxy value)
      let allKeys   = [minBound..maxBound :: key]
          mkPair k  =  (keyToText k, valueRef)
      return $ mempty
        & type_ .~ OpenAPIObject
        & properties .~ InsOrdHashMap.fromList (map mkPair allKeys)

-- | A 'Schema' for a mapping with 'Bounded' 'Enum' keys.
-- This makes a much more useful schema when there aren't many options for key values.
--
-- >>> data ButtonState = Neutral | Focus | Active | Hover | Disabled deriving (Show, Bounded, Enum, Generic)
-- >>> instance ToJSON ButtonState
-- >>> instance ToSchema ButtonState
-- >>> instance ToJSONKey ButtonState where toJSONKey = toJSONKeyText (T.pack . show)
-- >>> type ImageUrl = T.Text
-- >>> encode $ toSchemaBoundedEnumKeyMapping (Proxy :: Proxy (Map ButtonState ImageUrl))
-- "{\"properties\":{\"Neutral\":{\"type\":\"string\"},\"Focus\":{\"type\":\"string\"},\"Active\":{\"type\":\"string\"},\"Hover\":{\"type\":\"string\"},\"Disabled\":{\"type\":\"string\"}},\"type\":\"object\"}"
--
-- Note: this is only useful when @key@ is encoded with 'ToJSONKeyText'.
-- If it is encoded with 'ToJSONKeyValue' then a regular schema for @[(key, value)]@ is used.
toSchemaBoundedEnumKeyMapping :: forall map key value proxy.
  (Bounded key, Enum key, ToJSONKey key, ToSchema key, ToSchema value)
  => proxy (map key value) -> Schema
toSchemaBoundedEnumKeyMapping = flip evalDeclare mempty . declareSchemaBoundedEnumKeyMapping

-- | A configurable generic @'Schema'@ creator.
genericDeclareSchema :: (Generic a, GToSchema (Rep a), TypeHasSimpleShape a "genericDeclareSchemaUnrestricted") =>
  SchemaOptions -> proxy a -> Declare (Definitions Schema) Schema
genericDeclareSchema = genericDeclareSchemaUnrestricted

-- | A configurable generic @'NamedSchema'@ creator.
-- This function applied to @'defaultSchemaOptions'@
-- is used as the default for @'declareNamedSchema'@
-- when the type is an instance of @'Generic'@.
genericDeclareNamedSchema :: forall a proxy. (Generic a, GToSchema (Rep a), TypeHasSimpleShape a "genericDeclareNamedSchemaUnrestricted") =>
  SchemaOptions -> proxy a -> Declare (Definitions Schema) NamedSchema
genericDeclareNamedSchema = genericDeclareNamedSchemaUnrestricted

-- | A configurable generic @'Schema'@ creator.
--
-- Unlike 'genericDeclareSchema' also works for mixed sum types.
-- Use with care since some OpenAPI tools do not support well schemas for mixed sum types.
genericDeclareSchemaUnrestricted :: (Generic a, GToSchema (Rep a)) => SchemaOptions -> proxy a -> Declare (Definitions Schema) Schema
genericDeclareSchemaUnrestricted opts proxy = _namedSchemaSchema <$> genericDeclareNamedSchemaUnrestricted opts proxy

-- | A configurable generic @'NamedSchema'@ creator.
--
-- Unlike 'genericDeclareNamedSchema' also works for mixed sum types.
-- Use with care since some OpenAPI tools do not support well schemas for mixed sum types.
genericDeclareNamedSchemaUnrestricted :: forall a proxy. (Generic a, GToSchema (Rep a)) =>
  SchemaOptions -> proxy a -> Declare (Definitions Schema) NamedSchema
genericDeclareNamedSchemaUnrestricted opts _ = gdeclareNamedSchema opts (Proxy :: Proxy (Rep a)) mempty

-- | Derive a 'Generic'-based name for a datatype and assign it to a given 'Schema'.
genericNameSchema :: forall a d f proxy.
  (Generic a, Rep a ~ D1 d f, Datatype d)
  => SchemaOptions -> proxy a -> Schema -> NamedSchema
genericNameSchema opts _ = NamedSchema (gdatatypeSchemaName opts (Proxy :: Proxy d))

gdatatypeSchemaName :: forall proxy d. Datatype d => SchemaOptions -> proxy d -> Maybe T.Text
gdatatypeSchemaName opts _ = case orig of
  (c:_) | isAlpha c && isUpper c -> Just (T.pack name)
  _ -> Nothing
  where
    orig = datatypeName (Proxy3 :: Proxy3 d f a)
    name = datatypeNameModifier opts orig

-- | Lift a plain @'ParamSchema'@ into a model @'NamedSchema'@.
paramSchemaToNamedSchema :: forall a d f proxy.
  (ToParamSchema a, Generic a, Rep a ~ D1 d f, Datatype d)
  => SchemaOptions -> proxy a -> NamedSchema
paramSchemaToNamedSchema opts proxy = genericNameSchema opts proxy (paramSchemaToSchema proxy)

-- | Lift a plain @'ParamSchema'@ into a model @'Schema'@.
paramSchemaToSchema :: forall a proxy. ToParamSchema a => proxy a -> Schema
paramSchemaToSchema _ = mempty & paramSchema .~ toParamSchema (Proxy :: Proxy a)

nullarySchema :: Schema
nullarySchema = mempty
  & type_ .~ OpenAPIArray
  & items ?~ OpenAPIItemsArray []

gtoNamedSchema :: GToSchema f => SchemaOptions -> proxy f -> NamedSchema
gtoNamedSchema opts proxy = undeclare $ gdeclareNamedSchema opts proxy mempty

gdeclareSchema :: GToSchema f => SchemaOptions -> proxy f -> Declare (Definitions Schema) Schema
gdeclareSchema opts proxy = _namedSchemaSchema <$> gdeclareNamedSchema opts proxy mempty

instance (GToSchema f, GToSchema g) => GToSchema (f :*: g) where
  gdeclareNamedSchema opts _ schema = do
    NamedSchema _ gschema <- gdeclareNamedSchema opts (Proxy :: Proxy f) schema
    gdeclareNamedSchema opts (Proxy :: Proxy g) gschema

instance (Datatype d, GToSchema f) => GToSchema (D1 d f) where
  gdeclareNamedSchema opts _ s = rename name <$> gdeclareNamedSchema opts (Proxy :: Proxy f) s
    where
      name = gdatatypeSchemaName opts (Proxy :: Proxy d)

instance OVERLAPPABLE_ GToSchema f => GToSchema (C1 c f) where
  gdeclareNamedSchema opts _ = gdeclareNamedSchema opts (Proxy :: Proxy f)

instance OVERLAPPING_ Constructor c => GToSchema (C1 c U1) where
  gdeclareNamedSchema = gdeclareNamedSumSchema

-- | Single field constructor.
instance (Selector s, GToSchema f) => GToSchema (C1 c (S1 s f)) where
  gdeclareNamedSchema opts _ s
    | unwrapUnaryRecords opts = fieldSchema
    | otherwise =
        case schema ^. items of
          Just (OpenAPIItemsArray [_]) -> fieldSchema
          _ -> do
            declare defs
            return (unnamed schema)
    where
      (defs, NamedSchema _ schema) = runDeclare recordSchema mempty
      recordSchema = gdeclareNamedSchema opts (Proxy :: Proxy (S1 s f)) s
      fieldSchema  = gdeclareNamedSchema opts (Proxy :: Proxy f) s

gdeclareSchemaRef :: GToSchema a => SchemaOptions -> proxy a -> Declare (Definitions Schema) (Referenced Schema)
gdeclareSchemaRef opts proxy = do
  case gtoNamedSchema opts proxy of
    NamedSchema (Just name) schema -> do
      -- This check is very important as it allows generically
      -- derive used definitions for recursive schemas.
      -- Lazy Declare monad allows toNamedSchema to ignore
      -- any declarations (which would otherwise loop) and
      -- retrieve the schema and its name to check if we
      -- have already declared it.
      -- If we have, we don't need to declare anything for
      -- this schema this time and thus simply return the reference.
      known <- looks (InsOrdHashMap.member name)
      when (not known) $ do
        declare [(name, schema)]
        void $ gdeclareNamedSchema opts proxy mempty
      return $ Ref (Reference name)
    _ -> Inline <$> gdeclareSchema opts proxy

appendItem :: Referenced Schema -> Maybe (OpenAPIItems 'OpenAPIKindSchema) -> Maybe (OpenAPIItems 'OpenAPIKindSchema)
appendItem x Nothing = Just (OpenAPIItemsArray [x])
appendItem x (Just (OpenAPIItemsArray xs)) = Just (OpenAPIItemsArray (xs ++ [x]))
appendItem _ _ = error "GToSchema.appendItem: cannot append to OpenAPIItemsObject"

withFieldSchema :: forall proxy s f. (Selector s, GToSchema f) =>
  SchemaOptions -> proxy s f -> Bool -> Schema -> Declare (Definitions Schema) Schema
withFieldSchema opts _ isRequiredField schema = do
  ref <- gdeclareSchemaRef opts (Proxy :: Proxy f)
  return $
    if T.null fname
      then schema
        & type_ .~ OpenAPIArray
        & items %~ appendItem ref
        & maxItems %~ Just . maybe 1 (+1)   -- increment maxItems
        & minItems %~ Just . maybe 1 (+1)   -- increment minItems
      else schema
        & type_ .~ OpenAPIObject
        & properties . at fname ?~ ref
        & if isRequiredField
            then required %~ (++ [fname])
            else id
  where
    fname = T.pack (fieldLabelModifier opts (selName (Proxy3 :: Proxy3 s f p)))

-- | Optional record fields.
instance OVERLAPPING_ (Selector s, ToSchema c) => GToSchema (S1 s (K1 i (Maybe c))) where
  gdeclareNamedSchema opts _ = fmap unnamed . withFieldSchema opts (Proxy2 :: Proxy2 s (K1 i (Maybe c))) False

-- | Record fields.
instance OVERLAPPABLE_ (Selector s, GToSchema f) => GToSchema (S1 s f) where
  gdeclareNamedSchema opts _ = fmap unnamed . withFieldSchema opts (Proxy2 :: Proxy2 s f) True

instance OVERLAPPING_ ToSchema c => GToSchema (K1 i (Maybe c)) where
  gdeclareNamedSchema _ _ _ = declareNamedSchema (Proxy :: Proxy c)

instance OVERLAPPABLE_ ToSchema c => GToSchema (K1 i c) where
  gdeclareNamedSchema _ _ _ = declareNamedSchema (Proxy :: Proxy c)

instance ( GSumToSchema f
         , GSumToSchema g
         ) => GToSchema (f :+: g)
   where
  gdeclareNamedSchema = gdeclareNamedSumSchema

gdeclareNamedSumSchema :: GSumToSchema f => SchemaOptions -> proxy f -> Schema -> Declare (Definitions Schema) NamedSchema
gdeclareNamedSumSchema opts proxy s
  | allNullaryToStringTag opts && allNullary = pure $ unnamed (toStringTag sumSchema)
  | otherwise = (unnamed . fst) <$> runWriterT declareSumSchema
  where
    declareSumSchema = gsumToSchema opts proxy s
    (sumSchema, All allNullary) = undeclare (runWriterT declareSumSchema)

    toStringTag schema = mempty
      & type_ .~ OpenAPIString
      & enum_ ?~ map toJSON  (schema ^.. properties.ifolded.asIndex)

type AllNullary = All

class GSumToSchema (f :: * -> *)  where
  gsumToSchema :: SchemaOptions -> proxy f -> Schema -> WriterT AllNullary (Declare (Definitions Schema)) Schema

instance (GSumToSchema f, GSumToSchema g) => GSumToSchema (f :+: g) where
  gsumToSchema opts _ = gsumToSchema opts (Proxy :: Proxy f) >=> gsumToSchema opts (Proxy :: Proxy g)

gsumConToSchemaWith :: forall c f proxy. (GToSchema (C1 c f), Constructor c) =>
  Referenced Schema -> SchemaOptions -> proxy (C1 c f) -> Schema -> Schema
gsumConToSchemaWith ref opts _ schema = schema
  & type_ .~ OpenAPIObject
  & properties . at tag ?~ ref
  & maxProperties ?~ 1
  & minProperties ?~ 1
  where
    tag = T.pack (constructorTagModifier opts (conName (Proxy3 :: Proxy3 c f p)))

gsumConToSchema :: forall c f proxy. (GToSchema (C1 c f), Constructor c) =>
  SchemaOptions -> proxy (C1 c f) -> Schema -> Declare (Definitions Schema) Schema
gsumConToSchema opts proxy schema = do
  ref <- gdeclareSchemaRef opts proxy
  return $ gsumConToSchemaWith ref opts proxy schema

instance OVERLAPPABLE_ (Constructor c, GToSchema f) => GSumToSchema (C1 c f) where
  gsumToSchema opts proxy schema = do
    tell (All False)
    lift $ gsumConToSchema opts proxy schema

instance (Constructor c, Selector s, GToSchema f) => GSumToSchema (C1 c (S1 s f)) where
  gsumToSchema opts proxy schema = do
    tell (All False)
    lift $ gsumConToSchema opts proxy schema

instance Constructor c => GSumToSchema (C1 c U1) where
  gsumToSchema opts proxy = pure . gsumConToSchemaWith (Inline nullarySchema) opts proxy

data Proxy2 a b = Proxy2

data Proxy3 a b c = Proxy3

-- $setup
-- >>> import Data.OpenAPI
-- >>> import Data.Aeson (encode)
-- >>> import Data.Aeson.Types (toJSONKeyText)