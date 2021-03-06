{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}

module GitHub.Api (
    Repo (..), ErrorDescription (..), Commit (..), File(..), Person(..), CommitPerson(..), CommitPayload(..),
    Auth (..), RepoSource (..), Error (..), CommitsCriteria (..), CommitDetailsCriteria (..),
    fetchRepos, fetchCommits, fetchCommitDetails
    ) where

import           GHC.Generics (Generic)
import           Control.Arrow (left)
import           Control.Monad ((>=>))
import           Data.Monoid ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Encoding         as E
import qualified Data.Text.Lazy.Encoding    as LE
import qualified Data.ByteString.Lazy.Char8 as LS8
import qualified Data.ByteString.Char8      as S8
import qualified Network.HTTP.Simple        as HTTP
import qualified Data.Aeson                 as Aeson
import qualified Data.Time.Clock            as Clock
import qualified Data.Time.Format           as TimeFormat
import qualified Data.Maybe                 as Maybe


-- GitHub domain types

newtype ErrorDescription = ErrorDescription {
    message :: T.Text
} deriving (Generic, Show)

data Repo = Repo {
    id :: Int,
    owner :: Person,
    name :: T.Text,
    full_name :: T.Text
} deriving (Generic, Show)

newtype Person = Person {
    login :: T.Text
} deriving (Generic, Show)

data Commit = Commit {
    sha :: T.Text,
    commit :: CommitPayload,
    author :: Maybe Person,
    committer :: Maybe Person,
    files :: Maybe [File]
} deriving (Generic, Show)

data CommitPayload = CommitPayload {
    author :: CommitPerson,
    committer :: CommitPerson
} deriving (Generic, Show)

data CommitPerson = CommitPerson {
    name :: T.Text,
    email :: T.Text,
    date :: Clock.UTCTime
} deriving (Generic, Show)

data File = File {
    filename :: T.Text,
    additions :: Int,
    deletions :: Int
} deriving (Generic, Show)


-- API types

newtype Auth = Auth {
    token :: T.Text
} deriving (Show)

data Error =
    InvalidPayload {payload :: T.Text, parserError :: T.Text} |
    GitHubApiError {statusCode :: Int, errorDescription :: ErrorDescription} |
    OtherError {reason :: T.Text}
    deriving (Show)

data RepoSource = Own | User T.Text | Organization T.Text deriving (Show)

data CommitsCriteria = CommitsCriteria {
    repoFullName :: T.Text,
    since :: Maybe Clock.UTCTime,
    until :: Maybe Clock.UTCTime
} deriving (Show)

data CommitDetailsCriteria = CommitDetailsCriteria {
    repoFullName :: T.Text,
    commitSha :: T.Text
} deriving (Show)


-- Api

fetchRepos :: Auth -> RepoSource -> IO (Either Error [Repo])
fetchRepos auth source =
    fetchResource auth $ reposRequest source

fetchCommits :: Auth -> CommitsCriteria -> IO (Either Error [Commit])
fetchCommits auth criteria =
    fetchResource auth $ commitsRequest criteria

fetchCommitDetails :: Auth -> CommitDetailsCriteria -> IO (Either Error Commit)
fetchCommitDetails auth criteria =
    fetchResource auth $ commitDetailsRequest criteria



fetchResource :: (Aeson.FromJSON a, Show a) => Auth -> (HTTP.Request -> HTTP.Request) -> IO (Either Error a)
fetchResource auth request =
    performRequest $ request $ authenticatedRequest auth githubRequest

performRequest :: (Aeson.FromJSON a, Show a) => HTTP.Request -> IO (Either Error a)
performRequest request = fmap parseResponse (HTTP.httpLBS request)


-- Payloads fetching

reposRequest :: RepoSource -> HTTP.Request -> HTTP.Request
reposRequest source =
    HTTP.setRequestPath endpoint . HTTP.setRequestQueryString requestQueryString
    where
        endpoint = E.encodeUtf8 $ case source of
            Own               -> "/user/repos"
            User name         -> "/users/" <> name <> "/repos"
            Organization name -> "/orgs/" <> name <> "/repos"
        requestQueryString = [
                ("per_page", Just "100")
            ]

commitsRequest :: CommitsCriteria -> HTTP.Request -> HTTP.Request
commitsRequest criteria =
    HTTP.setRequestPath requestPath . HTTP.setRequestQueryString requestQueryString
    where
        requestPath = E.encodeUtf8 ("/repos/" <> repoFullName (criteria :: CommitsCriteria) <> "/commits")
        requestQueryString = withoutEmpty [
                ("since", formatTime <$> since criteria),
                ("until", formatTime <$> GitHub.Api.until criteria),
                ("per_page", Just "100")
            ]
        withoutEmpty = filter $ Maybe.isJust . snd

commitDetailsRequest :: CommitDetailsCriteria -> HTTP.Request -> HTTP.Request
commitDetailsRequest criteria =
    HTTP.setRequestPath . E.encodeUtf8 $
        "/repos/" <>
        repoFullName (criteria :: CommitDetailsCriteria) <>
        "/commits/" <>
        commitSha (criteria :: CommitDetailsCriteria)


authenticatedRequest :: Auth -> HTTP.Request -> HTTP.Request
authenticatedRequest auth =
    HTTP.addRequestHeader "Authorization" $ E.encodeUtf8 ("token " <> token auth)

githubRequest :: HTTP.Request
githubRequest =
    HTTP.setRequestHost "api.github.com" .
    HTTP.setRequestMethod "GET" .
    HTTP.setRequestHeaders [("User-Agent", "Velociraptor")] .
    HTTP.setRequestSecure True .
    HTTP.setRequestPort 443 $ HTTP.defaultRequest

formatTime :: Clock.UTCTime -> S8.ByteString
formatTime t =
    S8.pack $ TimeFormat.formatTime TimeFormat.defaultTimeLocale format t
    where format = "%Y-%m-%dT%H:%M:%SZ"


-- Response parsing

-- TODO: refactor me.
parseResponse :: (Aeson.FromJSON a, Show a) => HTTP.Response LS8.ByteString -> Either Error a
parseResponse response =
    let statusCode = HTTP.getResponseStatusCode response
        responseBody = HTTP.getResponseBody response
        parser 200  = parsePayload
        parser code = (parsePayload :: LS8.ByteString -> Either Error ErrorDescription) >=>
                   (\err -> Left GitHubApiError {statusCode = code, errorDescription = err})
    in  parser statusCode responseBody

parsePayload :: (Aeson.FromJSON a) => LS8.ByteString -> Either Error a
parsePayload json =
    left wrapAesonError (Aeson.eitherDecode json)
    where wrapAesonError aesonError = InvalidPayload {
        payload = LT.toStrict $ LE.decodeUtf8 json,
        parserError = T.pack aesonError}


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

instance Aeson.FromJSON CommitPayload
instance Aeson.ToJSON CommitPayload where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON CommitPerson
instance Aeson.ToJSON CommitPerson where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON File
instance Aeson.ToJSON File where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
