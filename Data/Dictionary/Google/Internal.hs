module Data.Dictionary.Google.Internal where

import           Control.Applicative
import           Data.Aeson                 (eitherDecode)
import           Data.Char                  (chr)
import           Data.List                  (dropWhileEnd)
import           Network.HTTP               (getRequest, getResponseBody, simpleHTTP)
import           Numeric                    (readHex)
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Dictionary.Google.Types

getResponse :: String -> IO (Either String Response)
getResponse word = do
    let url = "http://www.google.com/dictionary/json?callback=a&sl=en&tl=en&q=" ++ word
    fmap (decodeHex . trimResponse) (getJson url) >>= maybe badContents goodContents
  where
    -- | Trim off the boiler plate callback characters, because JSONP is returned.
    -- Hard-code "2" because "callback=a" is also hard-coded, so the first two
    -- characters are "a("
    trimResponse :: String -> String
    trimResponse = dropWhileEnd (/= '}') . drop 2

    badContents :: IO (Either String Response)
    badContents = return (Left "invalid hex code encountered")

    goodContents :: String -> IO (Either String Response)
    goodContents = return . eitherDecode . BS.pack

getJson :: String -> IO String
getJson url = simpleHTTP (getRequest url) >>= getResponseBody

decodeHex :: String -> Maybe String
decodeHex ('\\':'x':x:y:ys) =
    case readHex [x,y] of
        [(n,"")] -> fmap (chr n :) (decodeHex ys)
        _ -> Nothing
decodeHex (x:xs) = fmap (x:) (decodeHex xs)
decodeHex [] = Just []

-- | Write response json to the specified file for inspection.
writeResponseDebug :: String -> FilePath -> IO ()
writeResponseDebug word path = do
    let url = "http://www.google.com/dictionary/json?callback=a&sl=en&tl=en&q=" ++ word
    contents <- dropWhileEnd (/= '}') . drop 2 <$> getJson url
    writeFile path contents
