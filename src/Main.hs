import GitHub.Api

import Data.Time
import Data.Time.Format


main :: IO ()
main = do
    let token = "token here"
    let auth = Auth { token = token }

    myRepos <- repos auth Own
    print myRepos

    case myRepos of
        Right xs -> do
            let s = parseTimeM False defaultTimeLocale "%-d-%-m-%Y" "16-1-2017"
            let u = parseTimeM False defaultTimeLocale "%-d-%-m-%Y" "16-1-2019"

            case s of
                Just t -> print $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t
                _ -> print "not parsed"

            c <- commits auth CommitsCriteria { repoFullName = full_name . head $ xs, since = s, GitHub.Api.until = u }
            case c of
                Right cs -> do
                    mapM_ print cs
                    print "done"
                Left error -> print error
        Left error -> print error


printRepoCommits :: Repo -> IO ()
printRepoCommits repo = do
    return ()

    -- https://wiki.haskell.org/High-level_option_handling_with_GetOpt
