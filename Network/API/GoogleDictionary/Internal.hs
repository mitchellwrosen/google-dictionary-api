{-# LANGUAGE ScopedTypeVariables #-}

module Network.API.GoogleDictionary.Internal where

import           Control.Applicative        ((<$>))
import           Control.Exception          (SomeException, catch)
import           Data.Char                  (chr)
import           Data.List                  (dropWhileEnd)
import           Network.HTTP               (getRequest, rspBody, simpleHTTP)
import           Network.Stream             (ConnError)
import           Numeric                    (readHex)

getJson :: String -> IO (Either ConnError String)
getJson url = fmap rspBody <$> simpleHTTP (getRequest url)

decodeHex :: String -> Maybe String
decodeHex ('\\':'x':x:y:ys) =
    case readHex [x,y] of
        [(n,"")] -> fmap (chr n :) (decodeHex ys)
        _ -> Nothing
decodeHex (x:xs) = fmap (x:) (decodeHex xs)
decodeHex [] = Just []

-- Write response json to the specified file for inspection.
writeResponseDebug :: String -> FilePath -> IO ()
writeResponseDebug word path = do
    let url = "http://www.google.com/dictionary/json?callback=a&sl=en&tl=en&q=" ++ word
    getJson url >>= either (\_ -> return ()) (writeFile path . dropWhileEnd (/= '}') . drop 2)
