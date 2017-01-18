{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GitHub.Api (
    Repo, ErrorDescription,
    Auth (..), RepoSource (..), Error (..),
    repos) where

import           GHC.Generics (Generic)
import           Control.Arrow (left)
import           Control.Monad ((>=>))
import qualified Data.ByteString.Lazy.Char8 as LS8
import qualified Data.ByteString.Char8      as S8
import qualified Network.HTTP.Simple        as HTTP
import qualified Data.Aeson                 as Aeson

-- TODO: use Text

-- GitHub domain types

data Repo = Repo {
    id :: Int,
    owner :: Owner,
    name :: String,
    full_name :: String
} deriving (Generic, Show)

data Owner = Owner {
    login :: String
} deriving (Generic, Show)

data ErrorDescription = ErrorDescription {
    message :: String
} deriving (Generic, Show)


-- API types

data Auth = Auth {
    token :: String
} deriving (Show)

data RepoSource = Own | User String | Organization String deriving (Show)

data Error =
    InvalidPayload {payload :: String, parserError :: String} |
    GitHubApiError {statusCode :: Int, errorDescription :: ErrorDescription}
    deriving (Show)


-- Api

repos :: Auth -> RepoSource -> IO (Either Error [Repo])
repos auth source =
     HTTP.httpLBS request >>= return . parseResponse
--     HTTP.httpLBS request >>= print >> return (Right [])
    where request = reposRequest source $ authenticatedRequest auth githubRequest


-- Payloads fetching

bodyFromResponse :: HTTP.Response LS8.ByteString -> Either Error LS8.ByteString
bodyFromResponse response = pure $ HTTP.getResponseBody response

-- TODO: Should we move these strings to sort of constants?
reposRequest Own =
    let endpoint = "/user/repos"
    in  HTTP.setRequestPath endpoint
reposRequest (User name) =
    let endpoint a = "/users/" ++ a ++ "/repos"
    in  HTTP.setRequestPath . S8.pack . endpoint $ name
reposRequest (Organization name) =
    let endpoint a = "/orgs/" ++ a ++ "/repos"
    in  HTTP.setRequestPath . S8.pack . endpoint $ name

authenticatedRequest auth =
    HTTP.addRequestHeader "Authorization" (S8.pack ("token " ++ token auth))

githubRequest =
    HTTP.setRequestHost "api.github.com" .
    HTTP.setRequestMethod "GET" .
    HTTP.setRequestHeaders [("User-Agent", "Velociraptor")] .
    HTTP.setRequestSecure True .
    HTTP.setRequestPort 443 $ HTTP.defaultRequest


-- Response parsing

parseResponse :: (Aeson.FromJSON a) => HTTP.Response LS8.ByteString -> Either Error a
parseResponse response =
    let statusCode = HTTP.getResponseStatusCode response
        responseBody = bodyFromResponse response
        parser 200  = parsePayload
        parser code = (parsePayload :: LS8.ByteString -> Either Error ErrorDescription) >=>
                   (\err -> Left GitHubApiError {statusCode = code, errorDescription = err})
    in  responseBody >>= parser statusCode


parsePayload :: (Aeson.FromJSON a) => LS8.ByteString -> Either Error a
parsePayload json =
    left wrapAesonError (Aeson.eitherDecode json)
    where wrapAesonError aesonError = InvalidPayload {payload = LS8.unpack json, parserError = aesonError}


instance Aeson.FromJSON Repo
instance Aeson.ToJSON Repo where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Owner
instance Aeson.ToJSON Owner where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON ErrorDescription
instance Aeson.ToJSON ErrorDescription where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

