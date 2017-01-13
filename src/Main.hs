{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Data.ByteString.Lazy.Char8 as LS8
import qualified Data.ByteString.Char8      as S8
import           GHC.Generics
import           Data.Aeson
import           Network.HTTP.Simple

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


main :: IO ()
main = do

    -- https://haskell-lang.org/library/http-client
    let token = "github token here"

    let request
            = setRequestHost "api.github.com"
            $ setRequestMethod "GET"
            $ setRequestPath "/users/ysukhoverkhov/repos"
            $ setRequestHeaders [("Authorization", S8.pack("token " ++ token)), ("User-Agent", "Velociraptor")]
            $ setRequestSecure True
            $ setRequestPort 443
            $ defaultRequest
    response <- httpLBS request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    putStrLn $ "Content-Type" ++
               show (getResponseHeader "Content-Type" response)

    let body = getResponseBody response
    LS8.putStrLn body

    let repos = decode body :: Maybe [Repo]
    print repos

    let error = decode body :: Maybe GHError
    print error

    -- https://wiki.haskell.org/High-level_option_handling_with_GetOpt
