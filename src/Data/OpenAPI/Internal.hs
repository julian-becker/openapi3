{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ <710
{-# LANGUAGE PolyKinds #-}
#endif
#include "overlapping-compat.h"
module Data.OpenAPI.Internal where

import Prelude ()
import Prelude.Compat

import           Control.Lens             ((&), (.~), (?~))
import           Control.Applicative
import           Data.Aeson
import qualified Data.Aeson.Types         as JSON
import           Data.Data                (Data(..), Typeable, mkConstr, mkDataType, Fixity(..), Constr, DataType, constrIndex)
import qualified Data.HashMap.Strict      as HashMap
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Monoid
import           Data.Scientific          (Scientific)
import           Data.Set                 (Set)
import           Data.String              (IsString(..))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           GHC.Generics             (Generic)
import           Network                  (HostName, PortNumber)
import           Network.HTTP.Media       (MediaType)
import           Text.Read                (readMaybe)

import           Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap

import Generics.SOP.TH                  (deriveGeneric)
import Data.OpenAPI.Internal.AesonUtils (sopOpenAPIGenericToJSON
                                        ,sopOpenAPIGenericToJSONWithOpts
                                        ,sopOpenAPIGenericParseJSON
                                        ,HasOpenAPIAesonOptions(..)
                                        ,AesonDefaultValue(..)
                                        ,mkOpenAPIAesonOptions
                                        ,saoAdditionalPairs
                                        ,saoSubObject)
import Data.OpenAPI.Internal.Utils

#if MIN_VERSION_aeson(0,10,0)
import Data.OpenAPI.Internal.AesonUtils (sopOpenAPIGenericToEncoding)
#define DEFINE_TOENCODING toEncoding = sopOpenAPIGenericToEncoding
#else
#define DEFINE_TOENCODING
#endif

-- | A list of definitions that can be used in references.
type Definitions = InsOrdHashMap Text

-- | This is the root document object for the API specification.
data OpenAPI = OpenAPI
  { -- | Provides metadata about the API.
    -- The metadata can be used by the clients if needed.
    _openAPIInfo :: Info

    -- | The base path on which the API is served, which is relative to the host.
    -- If it is not included, the API is served directly under the host.
    -- The value MUST start with a leading slash (/).
  , _openAPIBasePath :: Maybe FilePath

    -- | The transfer protocol of the API.
    -- If the schemes is not included, the default scheme to be used is the one used to access the OpenAPI definition itself.
  , _openAPISchemes :: Maybe [Scheme]

    -- | A list of MIME types the APIs can consume.
    -- This is global to all APIs but can be overridden on specific API calls.
  , _openAPIConsumes :: MimeList

    -- | A list of MIME types the APIs can produce.
    -- This is global to all APIs but can be overridden on specific API calls.
  , _openAPIProduces :: MimeList

    -- | The available paths and operations for the API.
    -- Holds the relative paths to the individual endpoints.
    -- The path is appended to the @'basePath'@ in order to construct the full URL.
  , _openAPIPaths :: InsOrdHashMap FilePath PathItem

    -- | An object to hold data types produced and consumed by operations.
  , _openAPIDefinitions :: Definitions Schema

    -- | An object to hold parameters that can be used across operations.
    -- This property does not define global parameters for all operations.
  , _openAPIParameters :: Definitions Param

    -- | An object to hold responses that can be used across operations.
    -- This property does not define global responses for all operations.
  , _openAPIResponses :: Definitions Response

    -- | Security scheme definitions that can be used across the specification.
  , _openAPISecurityDefinitions :: Definitions SecurityScheme

    -- | A declaration of which security schemes are applied for the API as a whole.
    -- The list of values describes alternative security schemes that can be used
    -- (that is, there is a logical OR between the security requirements).
    -- Individual operations can override this definition.
  , _openAPISecurity :: [SecurityRequirement]

    -- | A list of tags used by the specification with additional metadata.
    -- The order of the tags can be used to reflect on their order by the parsing tools.
    -- Not all tags that are used by the Operation Object must be declared.
    -- The tags that are not declared may be organized randomly or based on the tools' logic.
    -- Each tag name in the list MUST be unique.
  , _openAPITags :: Set Tag

    -- | Additional external documentation.
  , _openAPIExternalDocs :: Maybe ExternalDocs
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | The object provides metadata about the API.
-- The metadata can be used by the clients if needed,
-- and can be presented in the OpenAPI-UI for convenience.
data Info = Info
  { -- | The title of the application.
    _infoTitle :: Text

    -- | A short description of the application.
    -- GFM syntax can be used for rich text representation.
  , _infoDescription :: Maybe Text

    -- | The Terms of Service for the API.
  , _infoTermsOfService :: Maybe Text

    -- | The contact information for the exposed API.
  , _infoContact :: Maybe Contact

    -- | The license information for the exposed API.
  , _infoLicense :: Maybe License

    -- | Provides the version of the application API
    -- (not to be confused with the specification version).
  , _infoVersion :: Text
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | Contact information for the exposed API.
data Contact = Contact
  { -- | The identifying name of the contact person/organization.
    _contactName  :: Maybe Text

    -- | The URL pointing to the contact information.
  , _contactUrl   :: Maybe URL

    -- | The email address of the contact person/organization.
  , _contactEmail :: Maybe Text
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | License information for the exposed API.
data License = License
  { -- | The license name used for the API.
    _licenseName :: Text

    -- | A URL to the license used for the API.
  , _licenseUrl :: Maybe URL
  } deriving (Eq, Show, Generic, Data, Typeable)

instance IsString License where
  fromString s = License (fromString s) Nothing

-- | The transfer protocol of the API.
data Scheme
  = Http
  | Https
  | Ws
  | Wss
  deriving (Eq, Show, Generic, Data, Typeable)

-- | Describes the operations available on a single path.
-- A @'PathItem'@ may be empty, due to ACL constraints.
-- The path itself is still exposed to the documentation viewer
-- but they will not know which operations and parameters are available.
data PathItem = PathItem
  { -- | A definition of a GET operation on this path.
    _pathItemGet :: Maybe Operation

    -- | A definition of a PUT operation on this path.
  , _pathItemPut :: Maybe Operation

    -- | A definition of a POST operation on this path.
  , _pathItemPost :: Maybe Operation

    -- | A definition of a DELETE operation on this path.
  , _pathItemDelete :: Maybe Operation

    -- | A definition of a OPTIONS operation on this path.
  , _pathItemOptions :: Maybe Operation

    -- | A definition of a HEAD operation on this path.
  , _pathItemHead :: Maybe Operation

    -- | A definition of a PATCH operation on this path.
  , _pathItemPatch :: Maybe Operation

    -- | A list of parameters that are applicable for all the operations described under this path.
    -- These parameters can be overridden at the operation level, but cannot be removed there.
    -- The list MUST NOT include duplicated parameters.
    -- A unique parameter is defined by a combination of a name and location.
  , _pathItemParameters :: [Referenced Param]
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | Describes a single API operation on a path.
data Operation = Operation
  { -- | A list of tags for API documentation control.
    -- Tags can be used for logical grouping of operations by resources or any other qualifier.
    _operationTags :: Set TagName

    -- | A short summary of what the operation does.
    -- For maximum readability in the openapi-ui, this field SHOULD be less than 120 characters.
  , _operationSummary :: Maybe Text

    -- | A verbose explanation of the operation behavior.
    -- GFM syntax can be used for rich text representation.
  , _operationDescription :: Maybe Text

    -- | Additional external documentation for this operation.
  , _operationExternalDocs :: Maybe ExternalDocs

    -- | Unique string used to identify the operation.
    -- The id MUST be unique among all operations described in the API.
    -- Tools and libraries MAY use the it to uniquely identify an operation,
    -- therefore, it is recommended to follow common programming naming conventions.
  , _operationOperationId :: Maybe Text

    -- | A list of MIME types the operation can consume.
    -- This overrides the @'consumes'@.
    -- @Just []@ MAY be used to clear the global definition.
  , _operationConsumes :: Maybe MimeList

    -- | A list of MIME types the operation can produce.
    -- This overrides the @'produces'@.
    -- @Just []@ MAY be used to clear the global definition.
  , _operationProduces :: Maybe MimeList

    -- | A list of parameters that are applicable for this operation.
    -- If a parameter is already defined at the @'PathItem'@,
    -- the new definition will override it, but can never remove it.
    -- The list MUST NOT include duplicated parameters.
    -- A unique parameter is defined by a combination of a name and location.
  , _operationParameters :: [Referenced Param]

    -- | The list of possible responses as they are returned from executing this operation.
  , _operationResponses :: Responses

    -- | The transfer protocol for the operation.
    -- The value overrides @'schemes'@.
  , _operationSchemes :: Maybe [Scheme]

    -- | Declares this operation to be deprecated.
    -- Usage of the declared operation should be refrained.
    -- Default value is @False@.
  , _operationDeprecated :: Maybe Bool

    -- | A declaration of which security schemes are applied for this operation.
    -- The list of values describes alternative security schemes that can be used
    -- (that is, there is a logical OR between the security requirements).
    -- This definition overrides any declared top-level security.
    -- To remove a top-level security declaration, @Just []@ can be used.
  , _operationSecurity :: [SecurityRequirement]
  } deriving (Eq, Show, Generic, Data, Typeable)

newtype MimeList = MimeList { getMimeList :: [MediaType] }
  deriving (Eq, Show, Monoid, Typeable)

mimeListConstr :: Constr
mimeListConstr = mkConstr mimeListDataType "MimeList" ["getMimeList"] Prefix

mimeListDataType :: DataType
mimeListDataType = mkDataType "Data.OpenAPI.MimeList" [mimeListConstr]

instance Data MimeList where
  gunfold k z c = case constrIndex c of
    1 -> k (z (\xs -> MimeList (map fromString xs)))
    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type MimeList."
  toConstr (MimeList _) = mimeListConstr
  dataTypeOf _ = mimeListDataType

-- | Describes a single operation parameter.
-- A unique parameter is defined by a combination of a name and location.
data Param = Param
  { -- | The name of the parameter.
    -- Parameter names are case sensitive.
    _paramName :: Text

    -- | A brief description of the parameter.
    -- This could contain examples of use.
    -- GFM syntax can be used for rich text representation.
  , _paramDescription :: Maybe Text

    -- | Determines whether this parameter is mandatory.
    -- If the parameter is in "path", this property is required and its value MUST be true.
    -- Otherwise, the property MAY be included and its default value is @False@.
  , _paramRequired :: Maybe Bool

    -- | Parameter schema.
  , _paramSchema :: ParamAnySchema
  } deriving (Eq, Show, Generic, Data, Typeable)

data ParamAnySchema
  = ParamBody (Referenced Schema)
  | ParamOther ParamOtherSchema
  deriving (Eq, Show, Generic, Data, Typeable)

data ParamOtherSchema = ParamOtherSchema
  { -- | The location of the parameter.
    _paramOtherSchemaIn :: ParamLocation

    -- | Sets the ability to pass empty-valued parameters.
    -- This is valid only for either @'ParamQuery'@ or @'ParamFormData'@
    -- and allows you to send a parameter with a name only or an empty value.
    -- Default value is @False@.
  , _paramOtherSchemaAllowEmptyValue :: Maybe Bool

  , _paramOtherSchemaParamSchema :: ParamSchema 'OpenAPIKindParamOtherSchema
  } deriving (Eq, Show, Generic, Typeable, Data)

-- | Items for @'OpenAPIArray'@ schemas.
--
-- @'OpenAPIItemsPrimitive'@ should be used only for query params, headers and path pieces.
-- The @'CollectionFormat' t@ parameter specifies how elements of an array should be displayed.
-- Note that @fmt@ in @'OpenAPIItemsPrimitive' fmt schema@ specifies format for elements of type @schema@.
-- This is different from the original OpenAPI's <http://openapi.io/specification/#itemsObject Items Object>.
--
-- @'OpenAPIItemsObject'@ should be used to specify homogenous array @'Schema'@s.
--
-- @'OpenAPIItemsArray'@ should be used to specify tuple @'Schema'@s.
data OpenAPIItems t where
  OpenAPIItemsPrimitive :: Maybe (CollectionFormat k) -> ParamSchema k-> OpenAPIItems k
  OpenAPIItemsObject    :: Referenced Schema   -> OpenAPIItems 'OpenAPIKindSchema
  OpenAPIItemsArray     :: [Referenced Schema] -> OpenAPIItems 'OpenAPIKindSchema
  deriving (Typeable)

deriving instance Eq (OpenAPIItems t)
deriving instance Show (OpenAPIItems t)
--deriving instance Typeable (OpenAPIItems t)

openapiItemsPrimitiveConstr :: Constr
openapiItemsPrimitiveConstr = mkConstr openapiItemsDataType "OpenAPIItemsPrimitive" [] Prefix

openapiItemsObjectConstr :: Constr
openapiItemsObjectConstr = mkConstr openapiItemsDataType "OpenAPIItemsObject" [] Prefix

openapiItemsArrayConstr :: Constr
openapiItemsArrayConstr = mkConstr openapiItemsDataType "OpenAPIItemsArray" [] Prefix

openapiItemsDataType :: DataType
openapiItemsDataType = mkDataType "Data.OpenAPI.OpenAPIItems" [openapiItemsPrimitiveConstr]

-- Note: unfortunately we have to write these Data instances by hand,
-- to get better contexts / avoid duplicate name when using standalone deriving

instance Data t => Data (OpenAPIItems ('OpenAPIKindNormal t)) where
  -- TODO: define gfoldl
  gunfold k z c = case constrIndex c of
    1 -> k (k (z OpenAPIItemsPrimitive))
    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type (OpenAPIItems t)."
  toConstr _ = openapiItemsPrimitiveConstr
  dataTypeOf _ = openapiItemsDataType

-- OpenAPIItems OpenAPIKindParamOtherSchema can be constructed using OpenAPIItemsPrimitive only
instance Data (OpenAPIItems 'OpenAPIKindParamOtherSchema) where
  -- TODO: define gfoldl
  gunfold k z c = case constrIndex c of
    1 -> k (k (z OpenAPIItemsPrimitive))
    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type (OpenAPIItems OpenAPIKindParamOtherSchema)."
  toConstr _ = openapiItemsPrimitiveConstr
  dataTypeOf _ = openapiItemsDataType

instance Data (OpenAPIItems 'OpenAPIKindSchema) where
  gfoldl _ _ (OpenAPIItemsPrimitive _ _) = error $ " Data.Data.gfoldl: Constructor OpenAPIItemsPrimitive used to construct OpenAPIItems OpenAPIKindSchema"
  gfoldl k z (OpenAPIItemsObject ref)    = z OpenAPIItemsObject `k` ref
  gfoldl k z (OpenAPIItemsArray ref)     = z OpenAPIItemsArray `k` ref

  gunfold k z c = case constrIndex c of
    2 -> k (z OpenAPIItemsObject)
    3 -> k (z OpenAPIItemsArray)
    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type (OpenAPIItems OpenAPIKindSchema)."

  toConstr (OpenAPIItemsPrimitive _ _) = error "Not supported"
  toConstr (OpenAPIItemsObject _)      = openapiItemsObjectConstr
  toConstr (OpenAPIItemsArray _)       = openapiItemsArrayConstr

  dataTypeOf _ = openapiItemsDataType

-- | Type used as a kind to avoid overlapping instances.
data OpenAPIKind t
    = OpenAPIKindNormal t
    | OpenAPIKindParamOtherSchema
    | OpenAPIKindSchema
    deriving (Typeable)

deriving instance Typeable 'OpenAPIKindNormal
deriving instance Typeable 'OpenAPIKindParamOtherSchema
deriving instance Typeable 'OpenAPIKindSchema

type family OpenAPIKindType (k :: OpenAPIKind *) :: *
type instance OpenAPIKindType ('OpenAPIKindNormal t) = t
type instance OpenAPIKindType 'OpenAPIKindSchema = Schema
type instance OpenAPIKindType 'OpenAPIKindParamOtherSchema = ParamOtherSchema

data OpenAPIType t where
  OpenAPIString   :: OpenAPIType t
  OpenAPINumber   :: OpenAPIType t
  OpenAPIInteger  :: OpenAPIType t
  OpenAPIBoolean  :: OpenAPIType t
  OpenAPIArray    :: OpenAPIType t
  OpenAPIFile     :: OpenAPIType 'OpenAPIKindParamOtherSchema
  OpenAPINull     :: OpenAPIType 'OpenAPIKindSchema
  OpenAPIObject   :: OpenAPIType 'OpenAPIKindSchema
  deriving (Typeable)

deriving instance Eq (OpenAPIType t)
deriving instance Show (OpenAPIType t)

openapiTypeConstr :: Data (OpenAPIType t) => OpenAPIType t -> Constr
openapiTypeConstr t = mkConstr (dataTypeOf t) (show t) [] Prefix

openapiTypeDataType :: {- Data (OpenAPIType t) => -} OpenAPIType t -> DataType
openapiTypeDataType _ = mkDataType "Data.OpenAPI.OpenAPIType" openapiTypeConstrs

openapiCommonTypes :: [OpenAPIType k]
openapiCommonTypes = [OpenAPIString, OpenAPINumber, OpenAPIInteger, OpenAPIBoolean, OpenAPIArray]

openapiParamTypes :: [OpenAPIType 'OpenAPIKindParamOtherSchema]
openapiParamTypes = openapiCommonTypes ++ [OpenAPIFile]

openapiSchemaTypes :: [OpenAPIType 'OpenAPIKindSchema]
openapiSchemaTypes = openapiCommonTypes ++ [error "OpenAPIFile is invalid OpenAPIType Schema", OpenAPINull, OpenAPIObject]

openapiTypeConstrs :: [Constr]
openapiTypeConstrs = map openapiTypeConstr (openapiCommonTypes :: [OpenAPIType 'OpenAPIKindSchema])
  ++ [openapiTypeConstr OpenAPIFile, openapiTypeConstr OpenAPINull, openapiTypeConstr OpenAPIObject]

instance Typeable t => Data (OpenAPIType ('OpenAPIKindNormal t)) where
  gunfold = gunfoldEnum "OpenAPIType" openapiCommonTypes
  toConstr = openapiTypeConstr
  dataTypeOf = openapiTypeDataType

instance Data (OpenAPIType 'OpenAPIKindParamOtherSchema) where
  gunfold = gunfoldEnum "OpenAPIType ParamOtherSchema" openapiParamTypes
  toConstr = openapiTypeConstr
  dataTypeOf = openapiTypeDataType

instance Data (OpenAPIType 'OpenAPIKindSchema) where
  gunfold = gunfoldEnum "OpenAPIType Schema" openapiSchemaTypes
  toConstr = openapiTypeConstr
  dataTypeOf = openapiTypeDataType

data ParamLocation
  = -- | Parameters that are appended to the URL.
    -- For example, in @/items?id=###@, the query parameter is @id@.
    ParamQuery
    -- | Custom headers that are expected as part of the request.
  | ParamHeader
    -- | Used together with Path Templating, where the parameter value is actually part of the operation's URL.
    -- This does not include the host or base path of the API.
    -- For example, in @/items/{itemId}@, the path parameter is @itemId@.
  | ParamPath
    -- | Used to describe the payload of an HTTP request when either @application/x-www-form-urlencoded@
    -- or @multipart/form-data@ are used as the content type of the request
    -- (in OpenAPI's definition, the @consumes@ property of an operation).
    -- This is the only parameter type that can be used to send files, thus supporting the @'ParamFile'@ type.
    -- Since form parameters are sent in the payload, they cannot be declared together with a body parameter for the same operation.
    -- Form parameters have a different format based on the content-type used
    -- (for further details, consult <http://www.w3.org/TR/html401/interact/forms.html#h-17.13.4>).
  | ParamFormData
  deriving (Eq, Show, Generic, Data, Typeable)

type Format = Text

-- | Determines the format of the array.
data CollectionFormat t where
  -- Comma separated values: @foo,bar@.
  CollectionCSV :: CollectionFormat t
  -- Space separated values: @foo bar@.
  CollectionSSV :: CollectionFormat t
  -- Tab separated values: @foo\\tbar@.
  CollectionTSV :: CollectionFormat t
  -- Pipe separated values: @foo|bar@.
  CollectionPipes :: CollectionFormat t
  -- Corresponds to multiple parameter instances
  -- instead of multiple values for a single instance @foo=bar&foo=baz@.
  -- This is valid only for parameters in @'ParamQuery'@ or @'ParamFormData'@.
  CollectionMulti :: CollectionFormat 'OpenAPIKindParamOtherSchema
  deriving (Typeable)

deriving instance Eq (CollectionFormat t)
deriving instance Show (CollectionFormat t)

collectionFormatConstr :: CollectionFormat t -> Constr
collectionFormatConstr cf = mkConstr collectionFormatDataType (show cf) [] Prefix

collectionFormatDataType :: DataType
collectionFormatDataType = mkDataType "Data.OpenAPI.CollectionFormat" $
  map collectionFormatConstr collectionCommonFormats

collectionCommonFormats :: [CollectionFormat t]
collectionCommonFormats = [ CollectionCSV, CollectionSSV, CollectionTSV, CollectionPipes ]

instance Data t => Data (CollectionFormat ('OpenAPIKindNormal t)) where
  gunfold = gunfoldEnum "CollectionFormat" collectionCommonFormats
  toConstr = collectionFormatConstr
  dataTypeOf _ = collectionFormatDataType

deriving instance Data (CollectionFormat 'OpenAPIKindParamOtherSchema)

type ParamName = Text

data Schema = Schema
  { _schemaTitle :: Maybe Text
  , _schemaDescription :: Maybe Text
  , _schemaRequired :: [ParamName]

  , _schemaAllOf :: Maybe [Referenced Schema]
  , _schemaProperties :: InsOrdHashMap Text (Referenced Schema)
  , _schemaAdditionalProperties :: Maybe (Referenced Schema)

  , _schemaDiscriminator :: Maybe Text
  , _schemaReadOnly :: Maybe Bool
  , _schemaXml :: Maybe Xml
  , _schemaExternalDocs :: Maybe ExternalDocs
  , _schemaExample :: Maybe Value

  , _schemaMaxProperties :: Maybe Integer
  , _schemaMinProperties :: Maybe Integer

  , _schemaParamSchema :: ParamSchema 'OpenAPIKindSchema
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | A @'Schema'@ with an optional name.
-- This name can be used in references.
data NamedSchema = NamedSchema
  { _namedSchemaName :: Maybe Text
  , _namedSchemaSchema :: Schema
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | Regex pattern for @string@ type.
type Pattern = Text

data ParamSchema (t :: OpenAPIKind *) = ParamSchema
  { -- | Declares the value of the parameter that the server will use if none is provided,
    -- for example a @"count"@ to control the number of results per page might default to @100@
    -- if not supplied by the client in the request.
    -- (Note: "default" has no meaning for required parameters.)
    -- Unlike JSON Schema this value MUST conform to the defined type for this parameter.
    _paramSchemaDefault :: Maybe Value

  , _paramSchemaType :: OpenAPIType t
  , _paramSchemaFormat :: Maybe Format
  , _paramSchemaItems :: Maybe (OpenAPIItems t)
  , _paramSchemaMaximum :: Maybe Scientific
  , _paramSchemaExclusiveMaximum :: Maybe Bool
  , _paramSchemaMinimum :: Maybe Scientific
  , _paramSchemaExclusiveMinimum :: Maybe Bool
  , _paramSchemaMaxLength :: Maybe Integer
  , _paramSchemaMinLength :: Maybe Integer
  , _paramSchemaPattern :: Maybe Pattern
  , _paramSchemaMaxItems :: Maybe Integer
  , _paramSchemaMinItems :: Maybe Integer
  , _paramSchemaUniqueItems :: Maybe Bool
  , _paramSchemaEnum :: Maybe [Value]
  , _paramSchemaMultipleOf :: Maybe Scientific
  } deriving (Eq, Show, Generic, Typeable)

deriving instance (Typeable k, Data (OpenAPIType k), Data (OpenAPIItems k)) => Data (ParamSchema k)

data Xml = Xml
  { -- | Replaces the name of the element/attribute used for the described schema property.
    -- When defined within the @'OpenAPIItems'@ (items), it will affect the name of the individual XML elements within the list.
    -- When defined alongside type being array (outside the items),
    -- it will affect the wrapping element and only if wrapped is true.
    -- If wrapped is false, it will be ignored.
    _xmlName :: Maybe Text

    -- | The URL of the namespace definition.
    -- Value SHOULD be in the form of a URL.
  , _xmlNamespace :: Maybe Text

    -- | The prefix to be used for the name.
  , _xmlPrefix :: Maybe Text

    -- | Declares whether the property definition translates to an attribute instead of an element.
    -- Default value is @False@.
  , _xmlAttribute :: Maybe Bool

    -- | MAY be used only for an array definition.
    -- Signifies whether the array is wrapped
    -- (for example, @\<books\>\<book/\>\<book/\>\</books\>@)
    -- or unwrapped (@\<book/\>\<book/\>@).
    -- Default value is @False@.
    -- The definition takes effect only when defined alongside type being array (outside the items).
  , _xmlWrapped :: Maybe Bool
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | A container for the expected responses of an operation.
-- The container maps a HTTP response code to the expected response.
-- It is not expected from the documentation to necessarily cover all possible HTTP response codes,
-- since they may not be known in advance.
-- However, it is expected from the documentation to cover a successful operation response and any known errors.
data Responses = Responses
  { -- | The documentation of responses other than the ones declared for specific HTTP response codes.
    -- It can be used to cover undeclared responses.
   _responsesDefault :: Maybe (Referenced Response)

    -- | Any HTTP status code can be used as the property name (one property per HTTP status code).
    -- Describes the expected response for those HTTP status codes.
  , _responsesResponses :: InsOrdHashMap HttpStatusCode (Referenced Response)
  } deriving (Eq, Show, Generic, Data, Typeable)

type HttpStatusCode = Int

-- | Describes a single response from an API Operation.
data Response = Response
  { -- | A short description of the response.
    -- GFM syntax can be used for rich text representation.
    _responseDescription :: Text

    -- | A definition of the response structure.
    -- It can be a primitive, an array or an object.
    -- If this field does not exist, it means no content is returned as part of the response.
    -- As an extension to the Schema Object, its root type value may also be "file".
    -- This SHOULD be accompanied by a relevant produces mime-type.
  , _responseSchema :: Maybe (Referenced Schema)

    -- | A list of headers that are sent with the response.
  , _responseHeaders :: InsOrdHashMap HeaderName Header

    -- | An example of the response message.
  , _responseExamples :: Maybe Example
  } deriving (Eq, Show, Generic, Data, Typeable)

instance IsString Response where
  fromString s = Response (fromString s) Nothing mempty Nothing

type HeaderName = Text

data Header = Header
  { -- | A short description of the header.
    _headerDescription :: Maybe Text

  , _headerParamSchema :: ParamSchema ('OpenAPIKindNormal Header)
  } deriving (Eq, Show, Generic, Data, Typeable)

data Example = Example { getExample :: Map MediaType Value }
  deriving (Eq, Show, Generic, Typeable)

exampleConstr :: Constr
exampleConstr = mkConstr exampleDataType "Example" ["getExample"] Prefix

exampleDataType :: DataType
exampleDataType = mkDataType "Data.OpenAPI.Example" [exampleConstr]

instance Data Example where
  gunfold k z c = case constrIndex c of
    1 -> k (z (\m -> Example (Map.mapKeys fromString m)))
    _ -> error $ "Data.Data.gunfold: Constructor " ++ show c ++ " is not of type Example."
  toConstr (Example _) = exampleConstr
  dataTypeOf _ = exampleDataType

-- | The location of the API key.
data ApiKeyLocation
  = ApiKeyQuery
  | ApiKeyHeader
  deriving (Eq, Show, Generic, Data, Typeable)

data ApiKeyParams = ApiKeyParams
  { -- | The name of the header or query parameter to be used.
    _apiKeyName :: Text

    -- | The location of the API key.
  , _apiKeyIn :: ApiKeyLocation
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | The authorization URL to be used for OAuth2 flow. This SHOULD be in the form of a URL.
type AuthorizationURL = Text

-- | The token URL to be used for OAuth2 flow. This SHOULD be in the form of a URL.
type TokenURL = Text

data OAuth2Flow
  = OAuth2Implicit AuthorizationURL
  | OAuth2Password TokenURL
  | OAuth2Application TokenURL
  | OAuth2AccessCode AuthorizationURL TokenURL
  deriving (Eq, Show, Generic, Data, Typeable)

data OAuth2Params = OAuth2Params
  { -- | The flow used by the OAuth2 security scheme.
    _oauth2Flow :: OAuth2Flow

    -- | The available scopes for the OAuth2 security scheme.
  , _oauth2Scopes :: InsOrdHashMap Text Text
  } deriving (Eq, Show, Generic, Data, Typeable)

data SecuritySchemeType
  = SecuritySchemeBasic
  | SecuritySchemeApiKey ApiKeyParams
  | SecuritySchemeOAuth2 OAuth2Params
  deriving (Eq, Show, Generic, Data, Typeable)

data SecurityScheme = SecurityScheme
  { -- | The type of the security scheme.
    _securitySchemeType :: SecuritySchemeType

    -- | A short description for security scheme.
  , _securitySchemeDescription :: Maybe Text
  } deriving (Eq, Show, Generic, Data, Typeable)

-- | Lists the required security schemes to execute this operation.
-- The object can have multiple security schemes declared in it which are all required
-- (that is, there is a logical AND between the schemes).
newtype SecurityRequirement = SecurityRequirement
  { getSecurityRequirement :: InsOrdHashMap Text [Text]
  } deriving (Eq, Read, Show, Monoid, ToJSON, FromJSON, Data, Typeable)

-- | Tag name.
type TagName = Text

-- | Allows adding meta data to a single tag that is used by @Operation@.
-- It is not mandatory to have a @Tag@ per tag used there.
data Tag = Tag
  { -- | The name of the tag.
    _tagName :: TagName

    -- | A short description for the tag.
    -- GFM syntax can be used for rich text representation.
  , _tagDescription :: Maybe Text

    -- | Additional external documentation for this tag.
  , _tagExternalDocs :: Maybe ExternalDocs
  } deriving (Eq, Ord, Show, Generic, Data, Typeable)

instance IsString Tag where
  fromString s = Tag (fromString s) Nothing Nothing

-- | Allows referencing an external resource for extended documentation.
data ExternalDocs = ExternalDocs
  { -- | A short description of the target documentation.
    -- GFM syntax can be used for rich text representation.
    _externalDocsDescription :: Maybe Text

    -- | The URL for the target documentation.
  , _externalDocsUrl :: URL
  } deriving (Eq, Ord, Show, Generic, Data, Typeable)

-- | A simple object to allow referencing other definitions in the specification.
-- It can be used to reference parameters and responses that are defined at the top level for reuse.
newtype Reference = Reference { getReference :: Text }
  deriving (Eq, Show, Data, Typeable)

data Referenced a
  = Ref Reference
  | Inline a
  deriving (Eq, Show, Functor, Data, Typeable)

instance IsString a => IsString (Referenced a) where
  fromString = Inline . fromString

newtype URL = URL { getUrl :: Text } deriving (Eq, Ord, Show, ToJSON, FromJSON, Data, Typeable)

-- =======================================================================
-- Monoid instances
-- =======================================================================

instance Monoid OpenAPI where
  mempty = genericMempty
  mappend = genericMappend

instance Monoid Info where
  mempty = genericMempty
  mappend = genericMappend

instance Monoid Contact where
  mempty = genericMempty
  mappend = genericMappend

instance Monoid PathItem where
  mempty = genericMempty
  mappend = genericMappend

instance Monoid Schema where
  mempty = genericMempty
  mappend = genericMappend

instance Monoid (ParamSchema t) where
  mempty = genericMempty
  mappend = genericMappend

instance Monoid Param where
  mempty = genericMempty
  mappend = genericMappend

instance Monoid ParamOtherSchema where
  mempty = genericMempty
  mappend = genericMappend

instance Monoid Header where
  mempty = genericMempty
  mappend = genericMappend

instance Monoid Responses where
  mempty = genericMempty
  mappend = genericMappend

instance Monoid Response where
  mempty = genericMempty
  mappend = genericMappend

instance Monoid ExternalDocs where
  mempty = genericMempty
  mappend = genericMappend

instance Monoid Operation where
  mempty = genericMempty
  mappend = genericMappend

instance Monoid Example where
  mempty = genericMempty
  mappend = genericMappend

-- =======================================================================
-- OpenAPIMonoid helper instances
-- =======================================================================

instance OpenAPIMonoid Info
instance OpenAPIMonoid PathItem
instance OpenAPIMonoid Schema
instance OpenAPIMonoid (ParamSchema t)
instance OpenAPIMonoid Param
instance OpenAPIMonoid ParamOtherSchema
instance OpenAPIMonoid Responses
instance OpenAPIMonoid Response
instance OpenAPIMonoid ExternalDocs
instance OpenAPIMonoid Operation

instance OpenAPIMonoid MimeList
deriving instance OpenAPIMonoid URL

instance OpenAPIMonoid (OpenAPIType t) where
  openapiMempty = OpenAPIString
  openapiMappend _ y = y

instance OpenAPIMonoid ParamLocation where
  openapiMempty = ParamQuery
  openapiMappend _ y = y

instance OVERLAPPING_ OpenAPIMonoid (InsOrdHashMap FilePath PathItem) where
  openapiMempty = InsOrdHashMap.empty
  openapiMappend = InsOrdHashMap.unionWith mappend

instance Monoid a => OpenAPIMonoid (Referenced a) where
  openapiMempty = Inline mempty
  openapiMappend (Inline x) (Inline y) = Inline (x <> y)
  openapiMappend _ y = y

instance OpenAPIMonoid ParamAnySchema where
  openapiMempty = ParamOther openapiMempty
  openapiMappend (ParamBody x) (ParamBody y) = ParamBody (openapiMappend x y)
  openapiMappend (ParamOther x) (ParamOther y) = ParamOther (openapiMappend x y)
  openapiMappend _ y = y

-- =======================================================================
-- Simple Generic-based ToJSON instances
-- =======================================================================

instance ToJSON ParamLocation where
  toJSON = genericToJSON (jsonPrefix "Param")

instance ToJSON Info where
  toJSON = genericToJSON (jsonPrefix "Info")

instance ToJSON Contact where
  toJSON = genericToJSON (jsonPrefix "Contact")

instance ToJSON License where
  toJSON = genericToJSON (jsonPrefix "License")

instance ToJSON ApiKeyLocation where
  toJSON = genericToJSON (jsonPrefix "ApiKey")

instance ToJSON ApiKeyParams where
  toJSON = genericToJSON (jsonPrefix "apiKey")

instance ToJSON Scheme where
  toJSON = genericToJSON (jsonPrefix "")

instance ToJSON Tag where
  toJSON = genericToJSON (jsonPrefix "Tag")

instance ToJSON ExternalDocs where
  toJSON = genericToJSON (jsonPrefix "ExternalDocs")

instance ToJSON Xml where
  toJSON = genericToJSON (jsonPrefix "Xml")

-- =======================================================================
-- Simple Generic-based FromJSON instances
-- =======================================================================

instance FromJSON ParamLocation where
  parseJSON = genericParseJSON (jsonPrefix "Param")

instance FromJSON Info where
  parseJSON = genericParseJSON (jsonPrefix "Info")

instance FromJSON Contact where
  parseJSON = genericParseJSON (jsonPrefix "Contact")

instance FromJSON License where
  parseJSON = genericParseJSON (jsonPrefix "License")

instance FromJSON ApiKeyLocation where
  parseJSON = genericParseJSON (jsonPrefix "ApiKey")

instance FromJSON ApiKeyParams where
  parseJSON = genericParseJSON (jsonPrefix "apiKey")

instance FromJSON Scheme where
  parseJSON = genericParseJSON (jsonPrefix "")

instance FromJSON Tag where
  parseJSON = genericParseJSON (jsonPrefix "Tag")

instance FromJSON ExternalDocs where
  parseJSON = genericParseJSON (jsonPrefix "ExternalDocs")

-- =======================================================================
-- Manual ToJSON instances
-- =======================================================================

instance ToJSON OAuth2Flow where
  toJSON (OAuth2Implicit authUrl) = object
    [ "flow"             .= ("implicit" :: Text)
    , "authorizationUrl" .= authUrl ]
  toJSON (OAuth2Password tokenUrl) = object
    [ "flow"     .= ("password" :: Text)
    , "tokenUrl" .= tokenUrl ]
  toJSON (OAuth2Application tokenUrl) = object
    [ "flow"     .= ("application" :: Text)
    , "tokenUrl" .= tokenUrl ]
  toJSON (OAuth2AccessCode authUrl tokenUrl) = object
    [ "flow"             .= ("accessCode" :: Text)
    , "authorizationUrl" .= authUrl
    , "tokenUrl"         .= tokenUrl ]

instance ToJSON OAuth2Params where
  toJSON = sopOpenAPIGenericToJSON
  DEFINE_TOENCODING

instance ToJSON SecuritySchemeType where
  toJSON SecuritySchemeBasic
      = object [ "type" .= ("basic" :: Text) ]
  toJSON (SecuritySchemeApiKey params)
      = toJSON params
    <+> object [ "type" .= ("apiKey" :: Text) ]
  toJSON (SecuritySchemeOAuth2 params)
      = toJSON params
    <+> object [ "type" .= ("oauth2" :: Text) ]

instance ToJSON OpenAPI where
  toJSON = sopOpenAPIGenericToJSON
  DEFINE_TOENCODING

instance ToJSON SecurityScheme where
  toJSON = sopOpenAPIGenericToJSON
  DEFINE_TOENCODING

instance ToJSON Schema where
  toJSON = sopOpenAPIGenericToJSON
  DEFINE_TOENCODING

instance ToJSON Header where
  toJSON = sopOpenAPIGenericToJSON
  DEFINE_TOENCODING

instance ToJSON (ParamSchema t) => ToJSON (OpenAPIItems t) where
  toJSON (OpenAPIItemsPrimitive fmt schema) = object
    [ "collectionFormat" .= fmt
    , "items"            .= schema ]
  toJSON (OpenAPIItemsObject x) = object [ "items" .= x ]
  toJSON (OpenAPIItemsArray  x) = object [ "items" .= x ]

instance ToJSON MimeList where
  toJSON (MimeList xs) = toJSON (map show xs)

instance ToJSON Param where
  toJSON = sopOpenAPIGenericToJSON
  DEFINE_TOENCODING

instance ToJSON ParamAnySchema where
  toJSON (ParamBody s) = object [ "in" .= ("body" :: Text), "schema" .= s ]
  toJSON (ParamOther s) = toJSON s

instance ToJSON ParamOtherSchema where
  toJSON = sopOpenAPIGenericToJSON
  DEFINE_TOENCODING

instance ToJSON Responses where
  toJSON = sopOpenAPIGenericToJSON
  DEFINE_TOENCODING

instance ToJSON Response where
  toJSON = sopOpenAPIGenericToJSON
  DEFINE_TOENCODING

instance ToJSON Operation where
  toJSON = sopOpenAPIGenericToJSON
  DEFINE_TOENCODING

instance ToJSON PathItem where
  toJSON = sopOpenAPIGenericToJSON
  DEFINE_TOENCODING

instance ToJSON Example where
  toJSON = toJSON . Map.mapKeys show . getExample

instance ToJSON Reference where
  toJSON (Reference ref) = object [ "$ref" .= ref ]

referencedToJSON :: ToJSON a => Text -> Referenced a -> Value
referencedToJSON prefix (Ref (Reference ref)) = object [ "$ref" .= (prefix <> ref) ]
referencedToJSON _ (Inline x) = toJSON x

instance ToJSON (Referenced Schema)   where toJSON = referencedToJSON "#/definitions/"
instance ToJSON (Referenced Param)    where toJSON = referencedToJSON "#/parameters/"
instance ToJSON (Referenced Response) where toJSON = referencedToJSON "#/responses/"

instance ToJSON (OpenAPIType t) where
  toJSON OpenAPIArray   = "array"
  toJSON OpenAPIString  = "string"
  toJSON OpenAPIInteger = "integer"
  toJSON OpenAPINumber  = "number"
  toJSON OpenAPIBoolean = "boolean"
  toJSON OpenAPIFile    = "file"
  toJSON OpenAPINull    = "null"
  toJSON OpenAPIObject  = "object"

instance ToJSON (CollectionFormat t) where
  toJSON CollectionCSV   = "csv"
  toJSON CollectionSSV   = "ssv"
  toJSON CollectionTSV   = "tsv"
  toJSON CollectionPipes = "pipes"
  toJSON CollectionMulti = "multi"

instance ToJSON (ParamSchema k) where
  -- TODO: this is a bit fishy, why we need sub object only in `ToJSON`?
  toJSON = sopOpenAPIGenericToJSONWithOpts $
      mkOpenAPIAesonOptions "paramSchema" & saoSubObject ?~ "items"

-- =======================================================================
-- Manual FromJSON instances
-- =======================================================================

instance FromJSON OAuth2Flow where
  parseJSON (Object o) = do
    (flow :: Text) <- o .: "flow"
    case flow of
      "implicit"    -> OAuth2Implicit    <$> o .: "authorizationUrl"
      "password"    -> OAuth2Password    <$> o .: "tokenUrl"
      "application" -> OAuth2Application <$> o .: "tokenUrl"
      "accessCode"  -> OAuth2AccessCode
        <$> o .: "authorizationUrl"
        <*> o .: "tokenUrl"
      _ -> empty
  parseJSON _ = empty

instance FromJSON OAuth2Params where
  parseJSON = sopOpenAPIGenericParseJSON

instance FromJSON SecuritySchemeType where
  parseJSON js@(Object o) = do
    (t :: Text) <- o .: "type"
    case t of
      "basic"  -> pure SecuritySchemeBasic
      "apiKey" -> SecuritySchemeApiKey <$> parseJSON js
      "oauth2" -> SecuritySchemeOAuth2 <$> parseJSON js
      _ -> empty
  parseJSON _ = empty

instance FromJSON OpenAPI where
  parseJSON = sopOpenAPIGenericParseJSON

instance FromJSON SecurityScheme where
  parseJSON = sopOpenAPIGenericParseJSON

instance FromJSON Schema where
  parseJSON = sopOpenAPIGenericParseJSON

instance FromJSON Header where
  parseJSON = sopOpenAPIGenericParseJSON

instance (FromJSON (CollectionFormat ('OpenAPIKindNormal t)), FromJSON (ParamSchema ('OpenAPIKindNormal t))) => FromJSON (OpenAPIItems ('OpenAPIKindNormal t)) where
  parseJSON = withObject "OpenAPIItemsPrimitive" $ \o -> OpenAPIItemsPrimitive
    <$> o .:? "collectionFormat"
    <*> (o .: "items" >>= parseJSON)

instance FromJSON (OpenAPIItems 'OpenAPIKindParamOtherSchema) where
  parseJSON = withObject "OpenAPIItemsPrimitive" $ \o -> OpenAPIItemsPrimitive
    <$> o .:? "collectionFormat"
    <*> ((o .: "items" >>= parseJSON) <|> fail ("foo" ++ show o))

instance FromJSON (OpenAPIItems 'OpenAPIKindSchema) where
  parseJSON js@(Object _) = OpenAPIItemsObject <$> parseJSON js
  parseJSON js@(Array _)  = OpenAPIItemsArray  <$> parseJSON js
  parseJSON _ = empty

instance FromJSON MimeList where
  parseJSON js = (MimeList . map fromString) <$> parseJSON js

instance FromJSON Param where
  parseJSON = sopOpenAPIGenericParseJSON

instance FromJSON ParamAnySchema where
  parseJSON js@(Object o) = do
    (i :: Text) <- o .: "in"
    case i of
      "body" -> do
        schema <- o .: "schema"
        ParamBody <$> parseJSON schema
      _ -> ParamOther <$> parseJSON js
  parseJSON _ = empty

instance FromJSON ParamOtherSchema where
  parseJSON = sopOpenAPIGenericParseJSON

instance FromJSON Responses where
  parseJSON (Object o) = Responses
    <$> o .:? "default"
    <*> (parseJSON (Object (HashMap.delete "default" o)))
  parseJSON _ = empty

instance FromJSON Example where
  parseJSON js = do
    m <- parseJSON js
    pure $ Example (Map.mapKeys fromString m)

instance FromJSON Response where
  parseJSON = sopOpenAPIGenericParseJSON

instance FromJSON Operation where
  parseJSON = sopOpenAPIGenericParseJSON

instance FromJSON PathItem where
  parseJSON = sopOpenAPIGenericParseJSON

instance FromJSON Reference where
  parseJSON (Object o) = Reference <$> o .: "$ref"
  parseJSON _ = empty

referencedParseJSON :: FromJSON a => Text -> Value -> JSON.Parser (Referenced a)
referencedParseJSON prefix js@(Object o) = do
  ms <- o .:? "$ref"
  case ms of
    Nothing -> Inline <$> parseJSON js
    Just s  -> Ref <$> parseRef s
  where
    parseRef s = do
      case Text.stripPrefix prefix s of
        Nothing     -> fail $ "expected $ref of the form \"" <> Text.unpack prefix <> "*\", but got " <> show s
        Just suffix -> pure (Reference suffix)
referencedParseJSON _ _ = fail "referenceParseJSON: not an object"

instance FromJSON (Referenced Schema)   where parseJSON = referencedParseJSON "#/definitions/"
instance FromJSON (Referenced Param)    where parseJSON = referencedParseJSON "#/parameters/"
instance FromJSON (Referenced Response) where parseJSON = referencedParseJSON "#/responses/"

instance FromJSON Xml where
  parseJSON = genericParseJSON (jsonPrefix "xml")

instance FromJSON (OpenAPIType 'OpenAPIKindSchema) where
  parseJSON = parseOneOf [OpenAPIString, OpenAPIInteger, OpenAPINumber, OpenAPIBoolean, OpenAPIArray, OpenAPINull, OpenAPIObject]

instance FromJSON (OpenAPIType 'OpenAPIKindParamOtherSchema) where
  parseJSON = parseOneOf [OpenAPIString, OpenAPIInteger, OpenAPINumber, OpenAPIBoolean, OpenAPIArray, OpenAPIFile]

instance FromJSON (OpenAPIType ('OpenAPIKindNormal t)) where
  parseJSON = parseOneOf [OpenAPIString, OpenAPIInteger, OpenAPINumber, OpenAPIBoolean, OpenAPIArray]

instance FromJSON (CollectionFormat ('OpenAPIKindNormal t)) where
  parseJSON = parseOneOf [CollectionCSV, CollectionSSV, CollectionTSV, CollectionPipes]

-- NOTE: There aren't collections of 'Schema'
--instance FromJSON (CollectionFormat (OpenAPIKindSchema)) where
--  parseJSON = parseOneOf [CollectionCSV, CollectionSSV, CollectionTSV, CollectionPipes]

instance FromJSON (CollectionFormat 'OpenAPIKindParamOtherSchema) where
  parseJSON = parseOneOf [CollectionCSV, CollectionSSV, CollectionTSV, CollectionPipes, CollectionMulti]

instance (FromJSON (OpenAPIType ('OpenAPIKindNormal t)), FromJSON (OpenAPIItems ('OpenAPIKindNormal t))) => FromJSON (ParamSchema ('OpenAPIKindNormal t)) where
  parseJSON = sopOpenAPIGenericParseJSON
instance FromJSON (ParamSchema 'OpenAPIKindParamOtherSchema) where
  parseJSON = sopOpenAPIGenericParseJSON
instance FromJSON (ParamSchema 'OpenAPIKindSchema) where
  parseJSON = sopOpenAPIGenericParseJSON

-------------------------------------------------------------------------------
-- TH splices
-------------------------------------------------------------------------------

deriveGeneric ''Header
deriveGeneric ''OAuth2Params
deriveGeneric ''Operation
deriveGeneric ''Param
deriveGeneric ''ParamOtherSchema
deriveGeneric ''PathItem
deriveGeneric ''Response
deriveGeneric ''Responses
deriveGeneric ''SecurityScheme
deriveGeneric ''Schema
deriveGeneric ''ParamSchema
deriveGeneric ''OpenAPI

instance HasOpenAPIAesonOptions Header where
  openapiAesonOptions _ = mkOpenAPIAesonOptions "header" & saoSubObject ?~ "paramSchema"
instance HasOpenAPIAesonOptions OAuth2Params where
  openapiAesonOptions _ = mkOpenAPIAesonOptions "oauth2" & saoSubObject ?~ "flow"
instance HasOpenAPIAesonOptions Operation where
  openapiAesonOptions _ = mkOpenAPIAesonOptions "operation"
instance HasOpenAPIAesonOptions Param where
  openapiAesonOptions _ = mkOpenAPIAesonOptions "param" & saoSubObject ?~ "schema"
instance HasOpenAPIAesonOptions ParamOtherSchema where
  openapiAesonOptions _ = mkOpenAPIAesonOptions "paramOtherSchema" & saoSubObject ?~ "paramSchema"
instance HasOpenAPIAesonOptions PathItem where
  openapiAesonOptions _ = mkOpenAPIAesonOptions "pathItem"
instance HasOpenAPIAesonOptions Response where
  openapiAesonOptions _ = mkOpenAPIAesonOptions "response"
instance HasOpenAPIAesonOptions Responses where
  openapiAesonOptions _ = mkOpenAPIAesonOptions "responses" & saoSubObject ?~ "responses"
instance HasOpenAPIAesonOptions SecurityScheme where
  openapiAesonOptions _ = mkOpenAPIAesonOptions "securityScheme" & saoSubObject ?~ "type"
instance HasOpenAPIAesonOptions Schema where
  openapiAesonOptions _ = mkOpenAPIAesonOptions "schema" & saoSubObject ?~ "paramSchema"
instance HasOpenAPIAesonOptions OpenAPI where
  openapiAesonOptions _ = mkOpenAPIAesonOptions "openapi" & saoAdditionalPairs .~ [("openapi", "3.0")]

instance HasOpenAPIAesonOptions (ParamSchema ('OpenAPIKindNormal t)) where
  openapiAesonOptions _ = mkOpenAPIAesonOptions "paramSchema" & saoSubObject ?~ "items"
instance HasOpenAPIAesonOptions (ParamSchema 'OpenAPIKindParamOtherSchema) where
  openapiAesonOptions _ = mkOpenAPIAesonOptions "paramSchema" & saoSubObject ?~ "items"
-- NOTE: Schema doesn't have 'items' sub object!
instance HasOpenAPIAesonOptions (ParamSchema 'OpenAPIKindSchema) where
  openapiAesonOptions _ = mkOpenAPIAesonOptions "paramSchema"

instance AesonDefaultValue (ParamSchema s)
instance AesonDefaultValue OAuth2Flow
instance AesonDefaultValue Responses
instance AesonDefaultValue ParamAnySchema
instance AesonDefaultValue SecuritySchemeType
instance AesonDefaultValue (OpenAPIType a)
instance AesonDefaultValue MimeList where defaultValue = Just mempty
instance AesonDefaultValue Info
instance AesonDefaultValue ParamLocation
