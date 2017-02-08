{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}

import GitHub.Api
import Analysis

import Data.Time
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

    case myRepos of
        Right xs -> do
            printRepoCommits auth $ head xs
            printRepoVelocity auth $ head xs
        Left error -> print error

    print "Done!"


printRepoCommits :: Auth -> Repo -> IO ()
printRepoCommits auth repo = do
--     let s = parseTimeM False defaultTimeLocale "%-d-%-m-%Y" "16-1-2017"

    print "Fetching commits..."
    ecs <- fetchCommits auth CommitsCriteria { repoFullName = full_name repo, since = Nothing, GitHub.Api.until = Nothing }
    case ecs of
        Right cs -> do
            print "Commits Range..."
            print $ commitsDateRange cs
            printCommitDetails auth repo $ head cs
        Left error -> print error

printCommitDetails :: Auth -> Repo -> Commit -> IO ()
printCommitDetails auth repo commit = do
    print "Fetching single commit..."
    ec <- fetchCommitDetails auth CommitDetailsCriteria { repoFullName = full_name repo, commitSha = sha commit}
    case ec of
        Right c -> do
            print c
            print $ linesAddedToCommits [".coffee", ".json"] [c]

        Left error -> print error


printRepoVelocity :: Auth -> Repo -> IO ()
printRepoVelocity auth repo = do
    print "Repo Velocity..."
    commits <- fetchCommits auth CommitsCriteria { repoFullName = full_name repo, since = Nothing, GitHub.Api.until = Nothing }

    case commits of
        Right cs -> do
            let dateRange = commitsDateRange cs
            let interval = 7 * 24 * 3600

            let maybeRanges = dateRanges interval <$> dateRange
            print maybeRanges

            case maybeRanges of
                Nothing -> print "No date range"
                Just ranges -> printLines ranges

        Left e -> print e

    where
        printLines [] = print "Done"
        printLines (x:xs) = do
            lines <- linesAddedInRange auth repo x
            let textToPrint = (\l -> show l ++ " - " ++ rangeText x) <$> lines
            print textToPrint
            printLines xs

        rangeText range = show (fst range) ++ " " ++ show (snd range)



--  TODO: refactor me and use transformers

data RangeInfo = RangeInfo {
    lines :: Int,
    contributors :: Int
} deriving (Show)

-- TODO: parametrize with file lists

linesAddedInRange :: Auth -> Repo -> (Clock.UTCTime, Clock.UTCTime) -> IO (Either Error RangeInfo)
linesAddedInRange auth repo range = do
    eitherCommits <- commitsForRange
    either (return . Left) rangeInfoForCommits eitherCommits

    where
        commitsForRange :: IO (Either Error [Commit])
        commitsForRange = fetchCommits auth CommitsCriteria {
            repoFullName = full_name repo,
            since = Just $ fst range,
            GitHub.Api.until = Just $ snd range
        }

        -- TODO: split this into "fetching detailed commits" and "analyzing commits"
        rangeInfoForCommits :: [Commit] -> IO (Either Error RangeInfo)
        rangeInfoForCommits commits = do
            eitherDetailedCommits <- commitsDetails commits
            return $ composeRangeInfo <$> eitherDetailedCommits

        composeRangeInfo :: [Commit] -> RangeInfo
        composeRangeInfo commits =
            RangeInfo {
                lines = linesAddedToCommits [".coffee", ".json"] commits,
                contributors = authorsInCommits [".coffee", ".json"] commits
            }


        commitsDetails :: [Commit] -> IO (Either Error [Commit])
        commitsDetails commits = sequence <$> sequence (map fc commits)

        fc :: Commit -> IO (Either Error Commit)
        fc c = fetchCommitDetails auth CommitDetailsCriteria {repoFullName = full_name repo, commitSha = sha c}



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

