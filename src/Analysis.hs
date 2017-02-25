{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}

module Analysis (
    SourceExtension,
    RangeInfo(..),
    calculateRangeInfo,
    dateRanges
    ) where

import qualified Data.Text                      as T
import qualified Data.List                      as L
import qualified Data.Time.Clock                as Clock
import qualified GitHub.Api                     as GH
import qualified Control.Monad.Trans.Either     as ET


{-# ANN module ("HLint: ignore Evaluate"::String) #-}
{-# ANN module ("HLint: ignore Use ***"::String) #-}

data RangeInfo = RangeInfo {
    lines :: Int,
    contributors :: [T.Text],
    commits :: Int
} deriving (Show)

type SourceExtension = T.Text

calculateRangeInfo :: GH.Auth -> [GH.Repo] -> [SourceExtension] -> (Clock.UTCTime, Clock.UTCTime) -> IO (Either GH.Error RangeInfo)
calculateRangeInfo auth repos extensions range = do
    eitherInfos <- sequence (fmap (calculateRangeInfo2 auth repos extensions) ranges)
    return (fmap combineRanges (sequence eitherInfos))
    where
        ranges = take rangesCount (dateRanges duration (snd range))
        duration = 24 * 3600 :: Clock.NominalDiffTime
        rangesCount :: Int
        rangesCount = round . realToFrac $ (Clock.diffUTCTime (snd range) (fst range) / duration)

combineRanges :: [RangeInfo] -> RangeInfo
combineRanges ranges =
    RangeInfo {
        lines = foldr (\r c -> c + Analysis.lines r) 0 ranges,
        contributors = fmap head . L.group . L.sort . foldr (\r c -> c ++ Analysis.contributors r) [] $ ranges,
        commits = foldr (\r c -> c + Analysis.commits r) 0 ranges
    }

calculateRangeInfo2 :: GH.Auth -> [GH.Repo] -> [SourceExtension] -> (Clock.UTCTime, Clock.UTCTime) -> IO (Either GH.Error RangeInfo)
calculateRangeInfo2 auth repos extensions range = do
    eitherCommits <- allReposCommits repos
    return (composeRangeInfo <$> eitherCommits)

    where
        allReposCommits :: [GH.Repo] -> IO (Either GH.Error [GH.Commit])
        allReposCommits repos =
            fmap (fmap concat . sequence) (sequence (fmap repoCommits repos))

        repoCommits :: GH.Repo -> IO (Either GH.Error [GH.Commit])
        repoCommits repo = ET.runEitherT (ET.EitherT (commitsForRange auth repo range) >>= (ET.EitherT . commitsDetails auth repo))

        composeRangeInfo :: [GH.Commit] -> RangeInfo
        composeRangeInfo commits =
            RangeInfo {
                lines = linesAddedToCommits extensions commits,
                contributors = authorsInCommits extensions commits,
                commits = length commits
            }

commitsForRange :: GH.Auth -> GH.Repo -> (Clock.UTCTime, Clock.UTCTime) -> IO (Either GH.Error [GH.Commit])
commitsForRange auth repo range = GH.fetchCommits auth GH.CommitsCriteria {
    repoFullName = GH.full_name repo,
    since = Just $ fst range,
    GH.until = Just $ snd range
}

commitsDetails :: GH.Auth -> GH.Repo -> [GH.Commit] -> IO (Either GH.Error [GH.Commit])
commitsDetails auth repo commits = sequence <$> mapM fc commits
    where
        fc :: GH.Commit -> IO (Either GH.Error GH.Commit)
        fc c = GH.fetchCommitDetails auth GH.CommitDetailsCriteria {repoFullName = GH.full_name repo, commitSha = GH.sha c}


authorsInCommits :: [SourceExtension] -> [GH.Commit] -> [T.Text]
authorsInCommits extensions =
    fmap head . L.group . L.sort . fmap authorEmail . filter shouldUseCommit
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

dateRanges :: Clock.NominalDiffTime -> Clock.UTCTime -> [(Clock.UTCTime, Clock.UTCTime)]
dateRanges step startTime =
    map rangeNumber [0..]
    where
        rangeNumber :: Integer -> (Clock.UTCTime, Clock.UTCTime)
        rangeNumber n = (date (n + 1), date n)

        date :: Integer -> Clock.UTCTime
        date n = Clock.addUTCTime (step * fromInteger (negate n)) startTime

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

