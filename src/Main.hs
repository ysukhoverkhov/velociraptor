{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as LS8
import qualified Data.ByteString.Char8      as S8
-- import           Data.Aeson                 (Value)
-- import qualified Data.Yaml                  as Yaml
import           Network.HTTP.Simple

main :: IO ()
main = do

    -- https://haskell-lang.org/library/http-client
    let token = "github token here"

    let request
            = setRequestHost "api.github.com"
            $ setRequestMethod "GET"
            $ setRequestPath "/"
            $ setRequestHeaders [("Authorization", S8.pack("token " ++ token)), ("User-Agent", "Awesome-Octocat-App")]
            $ setRequestSecure True
            $ setRequestPort 443
            $ defaultRequest
    response <- httpLBS request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    putStrLn $ "Content-Type" ++
               show (getResponseHeader "Content-Type" response)
    LS8.putStrLn $ getResponseBody response





    -- https://wiki.haskell.org/High-level_option_handling_with_GetOpt
