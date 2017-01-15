{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Data.ByteString.Lazy.Char8 as LS8
import qualified Data.ByteString.Char8      as S8
import           GHC.Generics
import           Data.Aeson
import           Network.HTTP.Simple
import           Data.Either.Utils

data Repo = Repo {
      id :: Int,
      name :: String
    } deriving (Generic, Show)

instance FromJSON Repo
instance ToJSON Repo where
    toEncoding = genericToEncoding defaultOptions

data GHError = GHError {
    message :: String
} deriving (Generic, Show)

instance FromJSON GHError
instance ToJSON GHError where
    toEncoding = genericToEncoding defaultOptions


data Auth = Auth {
    token :: String
} deriving (Show)

data RepoSource = Own | User String | Organization String deriving (Show)

repos :: Auth -> RepoSource -> IO (Either String [Repo])
repos auth source =
    httpLBS request >>= (\response -> return (bodyFromResponse response >>= parseResponse))
    where request = reposRequest source $ authenticatedRequest auth githubRequest

-- TODO: make it parse responses of any kind
parseResponse :: LS8.ByteString -> Either String [Repo]
parseResponse response = maybeToEither "Unable to decode response body" (decode response :: Maybe [Repo]) -- TODO: introduce error here


-- TODO: check status code here and return Left if not 200. (getResponseStatusCode response)
bodyFromResponse :: Response LS8.ByteString -> Either String LS8.ByteString
bodyFromResponse response = Right $ getResponseBody response

-- Should we move these strings to sort of constants?
reposRequest Own =
    let endpoint = "/user/repos"
    in  setRequestPath endpoint
reposRequest (User name) =
    let endpoint a = "/users/" ++ a ++ "/repos"
    in  setRequestPath (S8.pack (endpoint name))
reposRequest (Organization name) =
    let endpoint a = "/orgs/" ++ a ++ "/repos"
    in  setRequestPath (S8.pack (endpoint name))

authenticatedRequest auth =
    addRequestHeader "Authorization" (S8.pack ("token " ++ token auth))

githubRequest =
    setRequestHost "api.github.com"
    $ setRequestMethod "GET"
    $ setRequestHeaders [("User-Agent", "Velociraptor")]
    $ setRequestSecure True
    $ setRequestPort 443 defaultRequest


main :: IO ()
main =
    let token = "insert token here"
    in  repos Auth { token = token } Own >>= print

    -- https://wiki.haskell.org/High-level_option_handling_with_GetOpt
