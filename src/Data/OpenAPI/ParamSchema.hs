-- |
-- Module:      Data.OpenAPI.ParamSchema
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Types and functions for working with OpenAPI parameter schema.
module Data.OpenAPI.ParamSchema (
  -- * Encoding
  ToParamSchema(..),

  -- * Generic schema encoding
  genericToParamSchema,
  toParamSchemaBoundedIntegral,

  -- * Schema templates
  passwordParamSchema,
  binaryParamSchema,
  byteParamSchema,

  -- * Generic encoding configuration
  SchemaOptions(..),
  defaultSchemaOptions,
) where

import Data.OpenAPI.Internal.ParamSchema
import Data.OpenAPI.SchemaOptions
