{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}

module Util (unsafePrint) where

import System.IO.Unsafe (unsafePerformIO)

unsafePrint :: (Show a) => a -> a
unsafePrint q =
    unsafePerformIO (zz q)
    where
        zz :: (Show a) => a -> IO a
        zz q = do
            print q
            print "---------"
            return q
