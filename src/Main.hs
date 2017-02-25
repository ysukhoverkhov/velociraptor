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

    let zz = myRepos >>= findRepo "locomote/cbt"
    either print (\rs -> printRepoVelocity auth [rs]) zz


printRepoVelocity :: Auth -> [Repo] -> IO ()
printRepoVelocity auth repos = do
    let extensions = [".coffee", ".js", ".rb"]
    print "Repo Velocity..."

    currentTime <- getCurrentTime

    -- TODO: do it until results exist.
    let ranges = take 24 (dateRanges (30 * 24 * 60 * 60) currentTime)
    printLines ranges

    where
        printLines [] = print "Done"
        printLines (x:xs) = do
            let extensions = [".coffee", ".js", ".rb"]
            info <- calculateRangeInfo auth repos extensions x
            let textToPrint = (\i -> infoText i ++ ", " ++ rangeText x) <$> info
            either print print textToPrint
            hFlush stdout
            printLines xs

        rangeText range = show (fst range) ++ ", " ++ show (snd range)
        infoText info = (show . Analysis.lines $ info) ++ ", " ++ (show . Analysis.commits $ info) ++ ", " ++ (show . length . Analysis.contributors $ info)


findRepo :: T.Text -> [Repo] -> Either Error Repo
findRepo repoName repos = maybe
    (Left OtherError {reason = "Repo not found"} )
    Right
    (L.find (\r -> full_name r == repoName) repos)
