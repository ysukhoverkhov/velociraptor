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
    commits <- fetchCommits auth CommitsCriteria { repoFullName = full_name repo, since = Nothing, GitHub.Api.until = Nothing }

    print commits

    either print doPrint commits

    where
        doPrint cs = do
              let dateRange = commitsDateRange cs
              let interval = 7 * 24 * 3600

              let maybeRanges = dateRanges interval <$> dateRange
              print maybeRanges

              case maybeRanges of
                  Nothing -> print "No date range"
                  Just ranges -> printLines ranges

        printLines [] = print "Done"
        printLines (x:xs) = do
            lines <- linesAddedInRange auth repo x [".coffee"]
            let textToPrint = (\l -> show l ++ " - " ++ rangeText x) <$> lines
            print textToPrint
            printLines xs

        rangeText range = show (fst range) ++ " " ++ show (snd range)


--  TODO: move me somewhere.
dateRanges :: NominalDiffTime -> (Clock.UTCTime, Clock.UTCTime) -> [(Clock.UTCTime, Clock.UTCTime)]
dateRanges step totalRange =
    ranges totalRange []
    where
        ranges :: (Clock.UTCTime, Clock.UTCTime) -> [(Clock.UTCTime, Clock.UTCTime)] -> [(Clock.UTCTime, Clock.UTCTime)]
        ranges totalRange run
            | fst totalRange >= snd totalRange = run
            | otherwise =
                let nextStartDate = addUTCTime step $ fst totalRange
                in  ranges (nextStartDate, snd totalRange) ((fst totalRange, nextStartDate):run)

dateRanges2 :: NominalDiffTime -> Clock.UTCTime -> [(Clock.UTCTime, Clock.UTCTime)]
dateRanges2 step startTime =
    map rangeNumber [0..]
    where
        rangeNumber :: Integer -> (Clock.UTCTime, Clock.UTCTime)
        rangeNumber n = (date (n + 1), date n)

        date :: Integer -> Clock.UTCTime
        date n = addUTCTime (step * fromInteger (negate n)) startTime
