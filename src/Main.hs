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
            let s = parseTimeM False defaultTimeLocale "%-d-%-m-%Y" "1-1-2012"
            let u = parseTimeM False defaultTimeLocale "%-d-%-m-%Y" "1-1-2015"

            case s of
                Just t -> print $ formatTime defaultTimeLocale "%Y/%m/%d %H:%M" t
                _ -> print "not parsed"

            c <- commits auth CommitsCriteria { repoFullName = full_name . head $ xs, since = s, GitHub.Api.until = u }
            print c
        Left error -> print error


printRepoCommits :: Repo -> IO ()
printRepoCommits repo = do
    return ()

    -- https://wiki.haskell.org/High-level_option_handling_with_GetOpt
