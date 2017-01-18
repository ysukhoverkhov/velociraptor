{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GitHub.Api (
    Repo (..), ErrorDescription (..),
    Auth (..), RepoSource (..), Error (..), CommitsCriteria (..),
    repos, commits) where

import           GHC.Generics (Generic)
import           Control.Arrow (left)
import           Control.Monad ((>=>))
import           Data.Monoid ((<>))
import qualified Data.ByteString.Lazy.Char8 as LS8
import qualified Data.ByteString.Char8      as S8
import qualified Network.HTTP.Simple        as HTTP
import qualified Data.Aeson                 as Aeson
import qualified Data.Time.Clock            as Clock

-- TODO: use Text

-- GitHub domain types

data ErrorDescription = ErrorDescription {
    message :: String
} deriving (Generic, Show)

data Repo = Repo {
    id :: Int,
    owner :: Person,
    name :: String,
    full_name :: String
} deriving (Generic, Show)

data Person = Person {
    login :: String
} deriving (Generic, Show)

data Commit = Commit {
    sha :: String,
    author :: Person
} deriving (Generic, Show)

-- TODO: switch to GHC 8.0
-- data CommitPerson = CommitPerson {
--     name :: String,
--     email :: String,
--     date :: String
-- } deriving (Generic, Show)

-- API types

data Auth = Auth {
    token :: String
} deriving (Show)

data Error =
    InvalidPayload {payload :: String, parserError :: String} |
    GitHubApiError {statusCode :: Int, errorDescription :: ErrorDescription}
    deriving (Show)

data RepoSource = Own | User String | Organization String deriving (Show)

data CommitsCriteria = CommitsCriteria {
    repoFullName :: String,
    since :: Maybe Clock.UTCTime,
    until :: Maybe Clock.UTCTime
} deriving (Show)


-- Api

repos :: Auth -> RepoSource -> IO (Either Error [Repo])
repos auth source =
    performRequest $ reposRequest source $ authenticatedRequest auth githubRequest

commits :: Auth -> CommitsCriteria -> IO (Either Error [Commit])
commits auth criteria =
    performRequest $ commitsRequest criteria $ authenticatedRequest auth githubRequest

performRequest :: (Aeson.FromJSON a) => HTTP.Request -> IO (Either Error a)
performRequest request = HTTP.httpLBS request >>= return . parseResponse


-- Payloads fetching

bodyFromResponse :: HTTP.Response LS8.ByteString -> Either Error LS8.ByteString
bodyFromResponse response = pure $ HTTP.getResponseBody response -- TODO: think about jumping into functor here.

reposRequest source =
    HTTP.setRequestPath endpoint
    where endpoint = case source of
            Own               -> "/user/repos"
            User name         -> "/users/" <> S8.pack name <> "/repos"
            Organization name -> "/orgs/" <> S8.pack name <> "/repos"


-- TODO: take time frame into account.
commitsRequest criteria =
    (HTTP.setRequestPath . S8.pack $ "/repos/" ++ repoFullName criteria ++ "/commits") .
    HTTP.setRequestQueryString [("since", Just $ S8.pack "2011-04-14T16:00:49Z")]

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


instance Aeson.FromJSON ErrorDescription
instance Aeson.ToJSON ErrorDescription where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Repo
instance Aeson.ToJSON Repo where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Person
instance Aeson.ToJSON Person where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Commit
instance Aeson.ToJSON Commit where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

