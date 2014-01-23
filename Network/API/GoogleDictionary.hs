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
type SoundUrl     = String

data Entry = Entry
    { entryWord :: !String
    , entryData :: [(PartOfSpeech, Definition, Maybe SoundUrl)]
    }

instance Show Entry where
    show (Entry word dat) = unlines (word : show' 1 dat)
      where
        show' :: Int -> [(PartOfSpeech, Definition, Maybe SoundUrl)] -> [String]
        show' n ((pos,def,soundUrl):xs) =
            let soundUrlText = case soundUrl of
                                    Just soundUrl' -> " (" ++ soundUrl' ++ ")"
                                    Nothing -> ""
            in (show n ++ ". (" ++ pos ++ ") " ++ def ++ soundUrlText) : show' (n+1) xs
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
        let pos      = primaryTermsToPartOfSpeech terms
            soundUrl = primaryTermsToSoundUrl terms
            defs     = pentriesToDefinitions pentries
            s        = [(pos,d,soundUrl) | d <- defs]
        in (\(Entry w dat) -> Entry w (s++dat))

primaryTermsToSoundUrl :: [Term] -> Maybe SoundUrl
primaryTermsToSoundUrl = f
  where
    f :: [Term] -> Maybe SoundUrl
    f = getFirst . mconcat . map (First . primaryTermToPartOfSpeech)

    primaryTermToPartOfSpeech :: Term -> Maybe SoundUrl
    primaryTermToPartOfSpeech (Term _ _ soundUrl TSound) = Just soundUrl
    primaryTermToPartOfSpeech _ = Nothing

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
