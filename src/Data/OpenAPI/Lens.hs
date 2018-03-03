{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#include "overlapping-compat.h"
-- |
-- Module:      Data.OpenAPI.Lens
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Lenses and prisms for OpenAPI.
module Data.OpenAPI.Lens where

import Control.Lens
import Data.Aeson (Value)
import Data.Scientific (Scientific)
import Data.OpenAPI.Internal
import Data.OpenAPI.Internal.Utils
import Data.Text (Text)

-- * Classy lenses

makeFields ''OpenAPI
makeFields ''Host
makeFields ''Info
makeFields ''Contact
makeFields ''License
makeLensesWith openapiFieldRules ''PathItem
makeFields ''Tag
makeFields ''Operation
makeFields ''Param
makeLensesWith openapiFieldRules ''ParamOtherSchema
makeFields ''Header
makeFields ''Schema
makeFields ''NamedSchema
makeLensesWith openapiFieldRules ''ParamSchema
makeFields ''Xml
makeLensesWith openapiFieldRules ''Responses
makeFields ''Response
makeLensesWith openapiFieldRules ''SecurityScheme
makeFields ''ApiKeyParams
makeFields ''OAuth2Params
makeFields ''ExternalDocs

-- * Prisms
-- ** 'ParamAnySchema' prisms
makePrisms ''ParamAnySchema
-- ** 'SecuritySchemeType' prisms
makePrisms ''SecuritySchemeType
-- ** 'Referenced' prisms
makePrisms ''Referenced

-- ** 'OpenAPIItems' prisms

_openAPIItemsArray :: Review (OpenAPIItems 'OpenAPIKindSchema) [Referenced Schema]
_openAPIItemsArray
  = unto (\x -> OpenAPIItemsArray x)
{- \x -> case x of
      OpenAPIItemsPrimitive c p -> Left (OpenAPIItemsPrimitive c p)
      OpenAPIItemsObject o      -> Left (OpenAPIItemsObject o)
      OpenAPIItemsArray a       -> Right a
-}

_openAPIItemsObject :: Review (OpenAPIItems 'OpenAPIKindSchema) (Referenced Schema)
_openAPIItemsObject
  = unto (\x -> OpenAPIItemsObject x)
{- \x -> case x of
      OpenAPIItemsPrimitive c p -> Left (OpenAPIItemsPrimitive c p)
      OpenAPIItemsObject o      -> Right o
      OpenAPIItemsArray a       -> Left (OpenAPIItemsArray a)
-}

_openAPIItemsPrimitive :: forall t p f. (Profunctor p, Bifunctor p, Functor f) => Optic' p f (OpenAPIItems t) (Maybe (CollectionFormat t), ParamSchema t)
_openAPIItemsPrimitive = unto (\(c, p) -> OpenAPIItemsPrimitive c p)

-- =============================================================
-- More helpful instances for easier access to schema properties

type instance Index Responses = HttpStatusCode
type instance Index Operation = HttpStatusCode

type instance IxValue Responses = Referenced Response
type instance IxValue Operation = Referenced Response

instance Ixed Responses where ix n = responses . ix n
instance At   Responses where at n = responses . at n

instance Ixed Operation where ix n = responses . ix n
instance At   Operation where at n = responses . at n

instance HasParamSchema NamedSchema (ParamSchema 'OpenAPIKindSchema) where paramSchema = schema.paramSchema

-- HasType instances
instance HasType Header (OpenAPIType ('OpenAPIKindNormal Header)) where type_ = paramSchema.type_
instance HasType Schema (OpenAPIType 'OpenAPIKindSchema) where type_ = paramSchema.type_
instance HasType NamedSchema (OpenAPIType 'OpenAPIKindSchema) where type_ = paramSchema.type_
instance HasType ParamOtherSchema (OpenAPIType 'OpenAPIKindParamOtherSchema) where type_ = paramSchema.type_

-- HasDefault instances
instance HasDefault Header (Maybe Value) where default_ = paramSchema.default_
instance HasDefault Schema (Maybe Value) where default_ = paramSchema.default_
instance HasDefault ParamOtherSchema (Maybe Value) where default_ = paramSchema.default_

-- OVERLAPPABLE instances

instance
#if __GLASGOW_HASKELL__ >= 710
  OVERLAPPABLE_
#endif
  HasParamSchema s (ParamSchema t)
  => HasFormat s (Maybe Format) where
  format = paramSchema.format

instance
#if __GLASGOW_HASKELL__ >= 710
  OVERLAPPABLE_
#endif
  HasParamSchema s (ParamSchema t)
  => HasItems s (Maybe (OpenAPIItems t)) where
  items = paramSchema.items

instance
#if __GLASGOW_HASKELL__ >= 710
  OVERLAPPABLE_
#endif
  HasParamSchema s (ParamSchema t)
  => HasMaximum s (Maybe Scientific) where
  maximum_ = paramSchema.maximum_

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasExclusiveMaximum s (Maybe Bool) where
  exclusiveMaximum = paramSchema.exclusiveMaximum

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasMinimum s (Maybe Scientific) where
  minimum_ = paramSchema.minimum_

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasExclusiveMinimum s (Maybe Bool) where
  exclusiveMinimum = paramSchema.exclusiveMinimum

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasMaxLength s (Maybe Integer) where
  maxLength = paramSchema.maxLength

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasMinLength s (Maybe Integer) where
  minLength = paramSchema.minLength

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasPattern s (Maybe Text) where
  pattern = paramSchema.pattern

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasMaxItems s (Maybe Integer) where
  maxItems = paramSchema.maxItems

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasMinItems s (Maybe Integer) where
  minItems = paramSchema.minItems

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasUniqueItems s (Maybe Bool) where
  uniqueItems = paramSchema.uniqueItems

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasEnum s (Maybe [Value]) where
  enum_ = paramSchema.enum_

instance OVERLAPPABLE_ HasParamSchema s (ParamSchema t)
  => HasMultipleOf s (Maybe Scientific) where
  multipleOf = paramSchema.multipleOf
