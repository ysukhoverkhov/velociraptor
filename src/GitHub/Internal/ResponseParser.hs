module GitHub.Internal.ResponseParser (parseResponse) where

import           Data.Either.Utils          (maybeToEither)
import qualified Data.ByteString.Lazy.Char8 as LS8
import qualified Data.Aeson                 as Aeson
import qualified GitHub.Types               as Types


-- TODO: make it parse responses of any kind
parseResponse :: LS8.ByteString -> Either String [Types.Repo]
parseResponse response = maybeToEither "Unable to decode response body" (Aeson.decode response :: Maybe [Types.Repo]) -- TODO: introduce error here


-- Implementation

instance Aeson.FromJSON Types.Repo
instance Aeson.ToJSON Types.Repo where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Types.Error
instance Aeson.ToJSON Types.Error where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
