{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics

import Data.OpenAPI
import Data.OpenAPI.Declare
import Data.OpenAPI.Lens
import Data.OpenAPI.Operation

type Username = Text

data UserSummary = UserSummary
  { summaryUsername :: Username
  , summaryUserid   :: Int
  } deriving (Generic)

instance ToSchema UserSummary where
  declareNamedSchema _ = do
    usernameSchema <- declareSchemaRef (Proxy :: Proxy Username)
    useridSchema   <- declareSchemaRef (Proxy :: Proxy Int)
    return $ NamedSchema (Just "UserSummary") $ mempty
      & type_ .~ OpenAPIObject
      & properties .~
          [ ("summaryUsername", usernameSchema )
          , ("summaryUserid"  , useridSchema   )
          ]
      & required .~ [ "summaryUsername"
                    , "summaryUserid"   ]


type Group = Text

data UserDetailed = UserDetailed
  { username :: Username
  , userid   :: Int
  , groups   :: [Group]
  } deriving (Generic, ToSchema)

newtype Package = Package { packageName :: Text }
  deriving (Generic, ToSchema)

hackageOpenAPI :: OpenAPI
hackageOpenAPI = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareHackageOpenAPI mempty

declareHackageOpenAPI :: Declare (Definitions Schema) OpenAPI
declareHackageOpenAPI = do
  -- param schemas
  let usernameParamSchema = toParamSchema (Proxy :: Proxy Username)

  -- responses
  userSummaryResponse   <- declareResponse (Proxy :: Proxy UserSummary)
  userDetailedResponse  <- declareResponse (Proxy :: Proxy UserDetailed)
  packagesResponse      <- declareResponse (Proxy :: Proxy [Package])

  return $ mempty
    & paths .~
        [ ("/users", mempty & get ?~ (mempty
            & produces ?~ MimeList ["application/json"]
            & at 200 ?~ Inline userSummaryResponse))
        , ("/user/{username}", mempty & get ?~ (mempty
            & produces ?~ MimeList ["application/json"]
            & parameters .~ [ Inline $ mempty
                & name .~ "username"
                & required ?~ True
                & schema .~ ParamOther (mempty
                    & in_ .~ ParamPath
                    & paramSchema .~ usernameParamSchema) ]
            & at 200 ?~ Inline userDetailedResponse))
        , ("/packages", mempty & get ?~ (mempty
            & produces ?~ MimeList ["application/json"]
            & at 200 ?~ Inline packagesResponse))
        ]

main :: IO ()
main = putStrLn . read . show . encode $ hackageOpenAPI

