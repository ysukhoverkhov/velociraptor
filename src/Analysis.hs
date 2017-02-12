{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}

module Analysis (
    SourceExtension,
    RangeInfo(..),
    calculateRangeInfo,
    commitsDateRange,
    linesAddedToCommits,
    authorsInCommits
    ) where

import qualified Data.Text                      as T
import qualified Data.List                      as L
import qualified Data.Time.Clock                as Clock
import qualified GitHub.Api                     as GH
import qualified Control.Monad.Trans.Either     as ET

{-# ANN module ("HLint: ignore Use ***"::String) #-}

data RangeInfo = RangeInfo {
    lines :: Int,
    contributors :: Int,
    commits :: Int
} deriving (Show)

type SourceExtension = T.Text


calculateRangeInfo :: GH.Auth -> GH.Repo -> (Clock.UTCTime, Clock.UTCTime) -> [SourceExtension] -> IO (Either GH.Error RangeInfo)
calculateRangeInfo auth repo range extensions = do
    eitherCommits <- ET.runEitherT (ET.EitherT commitsForRange >>= (ET.EitherT . commitsDetails))
    return (composeRangeInfo <$> eitherCommits)

    where
        commitsForRange :: IO (Either GH.Error [GH.Commit])
        commitsForRange = GH.fetchCommits auth GH.CommitsCriteria {
            repoFullName = GH.full_name repo,
            since = Just $ fst range,
            GH.until = Just $ snd range
        }

        commitsDetails :: [GH.Commit] -> IO (Either GH.Error [GH.Commit])
        commitsDetails commits = sequence <$> mapM fc commits

        fc :: GH.Commit -> IO (Either GH.Error GH.Commit)
        fc c = GH.fetchCommitDetails auth GH.CommitDetailsCriteria {repoFullName = GH.full_name repo, commitSha = GH.sha c}

        composeRangeInfo :: [GH.Commit] -> RangeInfo
        composeRangeInfo commits =
            RangeInfo {
                lines = linesAddedToCommits extensions commits,
                contributors = authorsInCommits extensions commits,
                commits = length commits
            }


authorsInCommits :: [SourceExtension] -> [GH.Commit] -> Int
authorsInCommits extensions =
    length . L.group . L.sort . fmap authorEmail . filter shouldUseCommit
    where
        shouldUseCommit GH.Commit {GH.files = Just files} = any (fileHasExtension extensions) files
        shouldUseCommit GH.Commit {GH.files = Nothing} = False

        authorEmail c = GH.email (author (GH.commit c))
        author = GH.author :: GH.CommitPayload -> GH.CommitPerson

linesAddedToCommits :: [SourceExtension] -> [GH.Commit] -> Int
linesAddedToCommits extensions
    = foldr (\ c r -> r + linesAddedToCommit extensions c) 0

linesAddedToCommit :: [SourceExtension] -> GH.Commit -> Int
linesAddedToCommit _ GH.Commit {GH.files = Nothing} = 0
linesAddedToCommit extensions GH.Commit {GH.files = Just files} =
    sum . map linesAddedToFile . filter (fileHasExtension extensions) $ files
    where
        linesAddedToFile f = GH.additions f - GH.deletions f

-- Utility to find out is the file has one of specified extensions.
fileHasExtension :: [SourceExtension] -> GH.File -> Bool
fileHasExtension extensions file =
    any (hasExtension $ GH.filename file) extensions
    where
        hasExtension filename ext = T.isSuffixOf ext filename

commitsDateRange :: [GH.Commit] -> Maybe (Clock.UTCTime, Clock.UTCTime)
commitsDateRange =
    foldr updateRange Nothing
    where
        updateRange c r = case r of
            Nothing -> Just (d, d)
            Just v -> Just (min d $ fst v, max d $ snd v)
            where d = commitDate c
        commitDate c = GH.date (commitAuthor (GH.commit c))
        commitAuthor = GH.author :: GH.CommitPayload -> GH.CommitPerson
