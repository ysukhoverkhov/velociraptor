{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}

import GitHub.Api
import Analysis

import Data.Time
import qualified Data.List                            as L
import Data.Time.Clock                      as Clock
import Data.Time.Format
import qualified Data.Text                  as T
import Control.Monad
import Control.Monad.IO.Class
import System.IO

-- https://wiki.haskell.org/High-level_option_handling_with_GetOpt

-- NOTE: please do not review this file, mess here is by intention.

main :: IO ()
main = do
    let token = "token here"
    let auth = Auth { token = token }

    myRepos <- fetchRepos auth Own
    print "Repos..."
    print myRepos

    let cbtRepo = myRepos >>= findRepo "locomote/cbt"
    print cbtRepo

    either print (printRepoVelocity auth) cbtRepo

    where
        findRepo :: T.Text -> [Repo] -> Either Error Repo
        findRepo repoName repos = maybe
            (Left OtherError {reason = "Repo not found"} )
            Right
            (L.find (\r -> full_name r == repoName) repos)


printRepoVelocity :: Auth -> Repo -> IO ()
printRepoVelocity auth repo = do
    print "Repo Velocity..."

    currentTime <- getCurrentTime

    -- TODO: do it until results exist.
    let ranges = take 1 (dateRanges (1 * 24 * 60 * 60) currentTime)

    printLines ranges

    where

        printLines [] = print "Done"
        printLines (x:xs) = do
            lines <- calculateRangeInfo auth [repo] x [".coffee"]
            let textToPrint = (\l -> show l ++ " - " ++ rangeText x) <$> lines
            print textToPrint
            hFlush stdout
            printLines xs

        rangeText range = show (fst range) ++ " - " ++ show (snd range)


--  TODO: move me somewhere.
dateRanges :: NominalDiffTime -> Clock.UTCTime -> [(Clock.UTCTime, Clock.UTCTime)]
dateRanges step startTime =
    map rangeNumber [0..]
    where
        rangeNumber :: Integer -> (Clock.UTCTime, Clock.UTCTime)
        rangeNumber n = (date (n + 1), date n)

        date :: Integer -> Clock.UTCTime
        date n = addUTCTime (step * fromInteger (negate n)) startTime
