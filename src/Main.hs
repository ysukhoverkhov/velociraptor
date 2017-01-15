import GitHub.Api

main :: IO ()
main =
    let token = "insert token here."
    in  repos Auth { token = token } Own >>= print

    -- https://wiki.haskell.org/High-level_option_handling_with_GetOpt
