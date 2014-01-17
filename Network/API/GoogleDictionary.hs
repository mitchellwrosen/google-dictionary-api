{-# LANGUAGE TemplateHaskell #-}

module Network.API.GoogleDictionary
    ( Definition
    , Entry(..)
    , PartOfSpeech
    , lookupWord
    , getResponse
    , module Network.API.GoogleDictionary.Types
    ) where

import           Data.Aeson                 (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List                  (dropWhileEnd)
import           Data.Maybe                 (catMaybes)
import           Data.Monoid                (First(..), mconcat)

import Network.API.GoogleDictionary.Internal
import Network.API.GoogleDictionary.Types

type PartOfSpeech = String
type Definition   = String

data Entry = Entry
    { entryWord :: !String
    , entryData :: [(PartOfSpeech, Definition)]
    }

instance Show Entry where
    show = unlines . show' 1 . entryData
      where
        show' :: Int -> [(PartOfSpeech, Definition)] -> [String]
        show' n ((pos,def):xs) = (show n ++ ". (" ++ pos ++ ") " ++ def) : show' (n+1) xs
        show' _ [] = []

lookupWord :: String -> IO (Maybe Entry)
lookupWord word = lookupWord' (const Nothing) (Just . makeEntry word) word

{-
lookupWordDebug :: String -> IO (Either String Entry)
lookupWordDebug word = lookupWord' Left (Right . makeEntry word) word
-}

lookupWord' :: (String -> a) -> (Response -> a) -> String -> IO a
lookupWord' left right = fmap (either left right) . getResponse

makeEntry :: String -> Response -> Entry
makeEntry word = makeEntryFromPrimaries word . responsePrimaries

makeEntryFromPrimaries :: String -> [Primary] -> Entry
makeEntryFromPrimaries word = foldr step (Entry word [])
  where
    step :: Primary -> Entry -> Entry
    step (Primary pentries terms _) =
        let pos  = primaryTermsToPartOfSpeech terms
            defs = pentriesToDefinitions pentries
            s    = [(pos,d) | d <- defs]
        in (\(Entry w dat) -> Entry w (s++dat))

primaryTermsToPartOfSpeech :: [Term] -> PartOfSpeech
primaryTermsToPartOfSpeech = maybe (error "primaryTermsToPartOfSpeech: no part of speech found") id . f
  where
    f :: [Term] -> Maybe PartOfSpeech
    f = getFirst . mconcat . map (First . primaryTermToPartOfSpeech)

    primaryTermToPartOfSpeech :: Term -> Maybe PartOfSpeech
    primaryTermToPartOfSpeech (Term (Just labels) _ _ TText) = Just (labelsToPartOfSpeech labels)
    primaryTermToPartOfSpeech _ = Nothing

labelsToPartOfSpeech :: [Label] -> PartOfSpeech
labelsToPartOfSpeech = maybe (error "labelsToPartOfSpeech: no part of speech found") id . f
  where
    f :: [Label] -> Maybe PartOfSpeech
    f = getFirst . mconcat . map (First . labelToPartOfSpeech)

    labelToPartOfSpeech :: Label -> Maybe PartOfSpeech
    labelToPartOfSpeech (Label pos (Just "Part-of-speech")) = Just pos
    labelToPartOfSpeech _ = Nothing

pentriesToDefinitions :: [PEntry] -> [Definition]
pentriesToDefinitions = concatMap f
  where
    f :: PEntry -> [Definition]
    f (PEntry _ terms PEMeaning) = pentryTermsToDefinitions terms
    f _ = []

pentryTermsToDefinitions :: [Term] -> [Definition]
pentryTermsToDefinitions = catMaybes . map f
  where
    f :: Term -> Maybe Definition
    f (Term _ _ def TText) = Just def
    f _ = Nothing

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
