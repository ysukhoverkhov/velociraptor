{-# LANGUAGE OverloadedStrings #-}

module GitHub.Api (
    Auth (..),
    RepoSource (..),
    repos) where


import qualified Data.ByteString.Lazy.Char8     as LS8
import qualified Data.ByteString.Char8          as S8
import qualified Network.HTTP.Simple            as HTTP
import qualified GitHub.Types                   as Types
import qualified GitHub.Internal.ResponseParser as Parser


-- Argument types

data Auth = Auth {
    token :: String
} deriving (Show)

data RepoSource = Own | User String | Organization String deriving (Show)


-- Public Api

repos :: Auth -> RepoSource -> IO (Either String [Types.Repo])
repos auth source =
    HTTP.httpLBS request >>= (\response -> return (bodyFromResponse response >>= Parser.parseResponse))
    where request = reposRequest source $ authenticatedRequest auth githubRequest


-- Implementation

-- TODO: check status code here and return Left if not 200. (getResponseStatusCode response)
bodyFromResponse :: HTTP.Response LS8.ByteString -> Either String LS8.ByteString
bodyFromResponse response = Right $ HTTP.getResponseBody response

-- TODO: Should we move these strings to sort of constants?
reposRequest Own =
    let endpoint = "/user/repos"
    in  HTTP.setRequestPath endpoint
reposRequest (User name) =
    let endpoint a = "/users/" ++ a ++ "/repos"
    in  name $ HTTP.setRequestPath . S8.pack . endpoint
reposRequest (Organization name) =
    let endpoint a = "/orgs/" ++ a ++ "/repos"
    in  name $ HTTP.setRequestPath . S8.pack . endpoint

authenticatedRequest auth =
    HTTP.addRequestHeader "Authorization" (S8.pack ("token " ++ token auth))

githubRequest =
    HTTP.setRequestHost "api.github.com"
    $ HTTP.setRequestMethod "GET"
    $ HTTP.setRequestHeaders [("User-Agent", "Velociraptor")]
    $ HTTP.setRequestSecure True
    $ HTTP.setRequestPort 443 HTTP.defaultRequest
