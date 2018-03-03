-- |
-- Module:      Data.OpenAPI.Schema
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Types and functions for working with OpenAPI schema.
module Data.OpenAPI.Schema (
  -- * Encoding
  ToSchema(..),
  declareSchema,
  declareSchemaRef,
  toSchema,
  toSchemaRef,
  schemaName,
  toInlinedSchema,

  -- * Generic schema encoding
  genericDeclareNamedSchema,
  genericDeclareSchema,
  genericDeclareNamedSchemaNewtype,
  genericNameSchema,

  -- ** 'Bounded' 'Integral'
  genericToNamedSchemaBoundedIntegral,
  toSchemaBoundedIntegral,

  -- ** 'Bounded' 'Enum' key mappings
  declareSchemaBoundedEnumKeyMapping,
  toSchemaBoundedEnumKeyMapping,

  -- ** Reusing 'ToParamSchema'
  paramSchemaToNamedSchema,
  paramSchemaToSchema,

  -- ** Unrestricted versions
  genericDeclareNamedSchemaUnrestricted,
  genericDeclareSchemaUnrestricted,

  -- * Schema templates
  passwordSchema,
  binarySchema,
  byteSchema,

  -- * Sketching @'Schema'@s using @'ToJSON'@
  sketchSchema,
  sketchStrictSchema,

  -- * Inlining @'Schema'@s
  inlineNonRecursiveSchemas,
  inlineAllSchemas,
  inlineSchemas,
  inlineSchemasWhen,

  -- * Generic encoding configuration
  SchemaOptions(..),
  defaultSchemaOptions,
) where

import Data.OpenAPI.Internal.Schema
import Data.OpenAPI.SchemaOptions
