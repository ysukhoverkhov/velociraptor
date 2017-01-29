import GitHub.Api
import Analysis

import Data.Time
import Data.Time.Clock                      as Clock
import Data.Time.Format
import qualified Data.Text                  as T

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
    ec <- fetchCommit auth CommitCriteria { repoFullName = full_name repo, commitSha = sha commit}
    case ec of
        Right c -> do
            print c
            print $ linesAdded ([".coffee"]) c

        Left error -> print error


printRepoVelocity :: Auth -> Repo -> IO ()
printRepoVelocity auth repo = do
    print "Repo Velocity..."
    commits <- fetchCommits auth CommitsCriteria { repoFullName = full_name repo, since = Nothing, GitHub.Api.until = Nothing }

    case commits of
        Right cs -> do
            let dateRange = commitsDateRange cs
            let interval = 7 * 24 * 3600

            let ranges = (\r -> dateRanges interval r) <$> dateRange
            print ranges

            let commitsInRange = (\r -> head r) <$> ranges
            print commitsInRange

        Left e -> print e


-- TODO: move me to the library


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

