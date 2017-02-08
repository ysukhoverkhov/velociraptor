{-# LANGUAGE OverloadedStrings, DeriveGeneric, DuplicateRecordFields #-}

module Analysis (
    SourceExtension,
    commitsDateRange,
    linesAddedToCommits
    ) where

import qualified Data.Text       as T
import qualified Data.Time.Clock as Clock
import qualified GitHub.Api      as GH

{-# ANN module ("HLint: ignore Use ***"::String) #-}

type SourceExtension = T.Text

authorsInCommits :: [SourceExtension] -> [GH.Commit] -> Int
authorsInCommits extensions commits =
    length (filter shouldUseCommit commits)
    where
        shouldUseCommit commit = True
        

linesAddedToCommits :: [SourceExtension] -> [GH.Commit] -> Int
linesAddedToCommits extensions
    = foldr (\ c r -> r + linesAddedToCommit extensions c) 0

linesAddedToCommit :: [SourceExtension] -> GH.Commit -> Int
linesAddedToCommit _ GH.Commit {GH.files = Nothing} = 0
linesAddedToCommit extensions GH.Commit {GH.files = Just files} =
    sum . map linesAddedToFile . filter shouldUseFile $ files
    where
        linesAddedToFile f = GH.additions f - GH.deletions f
        shouldUseFile f = any (hasExtension $ GH.filename f) extensions
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
