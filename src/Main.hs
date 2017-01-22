{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

import GitHub.Api

import Data.Time
import Data.Time.Format

-- https://wiki.haskell.org/High-level_option_handling_with_GetOpt
-- NOTE: please do not review this file, mess here is by intention.

main :: IO ()
main = do
    let token = "token here"
    let auth = Auth { token = token }

    myRepos <- fetchRepos auth Own
    print myRepos

    case myRepos of
        Right xs -> printRepoCommits auth $ head xs
        Left error -> print error


printRepoCommits :: Auth -> Repo -> IO ()
printRepoCommits auth repo = do
    let s = parseTimeM False defaultTimeLocale "%-d-%-m-%Y" "16-1-2017"
    let u = parseTimeM False defaultTimeLocale "%-d-%-m-%Y" "16-1-2019"

    case s of
        Just t -> print $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t
        _ -> print "not parsed"

    ecs <- fetchCommits auth CommitsCriteria { repoFullName = full_name repo, since = s, GitHub.Api.until = u }
    case ecs of
        Right cs -> do
            mapM_ print cs
            printCommitDetails auth repo $ head cs
        Left error -> print error


printCommitDetails :: Auth -> Repo -> Commit -> IO ()
printCommitDetails auth repo commit = do
    print "Fetching single commit..."
    ec <- fetchCommit auth CommitCriteria { repoFullName = full_name repo, commitSha = sha commit}
    case ec of
        Right c -> print c
        Left error -> print error

-- Subject to move to a module bellow.

-- linesAdded ::
