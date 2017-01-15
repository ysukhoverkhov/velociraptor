{-# LANGUAGE DeriveGeneric #-}

module GitHub.Types (Repo, Error) where

import GHC.Generics (Generic)


data Repo = Repo {
      id :: Int,
      name :: String
    } deriving (Generic, Show)

data Error = Error {
    message :: String
} deriving (Generic, Show)

