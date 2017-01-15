module GitHub.Internal.ResponseParser (parseResponse) where

import qualified Data.ByteString.Lazy.Char8 as LS8
import qualified Data.Aeson                 as Aeson
import qualified GitHub.Types               as Types

-- TODO: write README.

-- TODO: make it parse only our types
parseResponse :: (Aeson.FromJSON a) => LS8.ByteString -> Either String a
parseResponse = Aeson.eitherDecode


-- Implementation

instance Aeson.FromJSON Types.Repo
instance Aeson.ToJSON Types.Repo where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance Aeson.FromJSON Types.Error
instance Aeson.ToJSON Types.Error where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
