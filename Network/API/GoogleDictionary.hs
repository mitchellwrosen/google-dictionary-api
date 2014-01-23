{-# LANGUAGE TemplateHaskell #-}

module Network.API.GoogleDictionary
    ( Entry(..)
    , lookupWord
    , getResponse
    , module Network.API.GoogleDictionary.Types
    ) where

import           Control.Applicative        ((<$>))
import           Control.Lens
import           Control.Monad              (join)
import           Control.Monad.State
import           Data.Aeson                 (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List                  (dropWhileEnd)
import           Data.Maybe                 (catMaybes)
import           Data.Monoid                (First(..), mconcat)

import Network.API.GoogleDictionary.Internal
import Network.API.GoogleDictionary.Types

data Entry = Entry
    { entryWord         :: !String
    , entryDefinition   :: !String
    , entryPartOfSpeech :: Maybe String
    , entryPhonetic     :: !String
    , entrySoundUrl     :: Maybe String
    } deriving Show

-- Internal representation of an Entry that is more similar to a Response.
data EntryInternal = EntryInternal
    { _eiDefinitions  :: [String]
    , _eiPartOfSpeech :: Maybe String
    , _eiPhonetic     :: !String
    , _eiSoundUrl     :: Maybe String
    } deriving Show
makeLenses ''EntryInternal

initEntryInternal :: EntryInternal
initEntryInternal = EntryInternal
    { _eiDefinitions  = []
    , _eiPartOfSpeech = Nothing
    , _eiPhonetic     = ""
    , _eiSoundUrl     = Nothing
    }

entryInternalToEntries :: String -> EntryInternal -> [Entry]
entryInternalToEntries word (EntryInternal defs pos phon sound) = 
    map (\def -> Entry word def pos phon sound) defs

{-instance Show Entry where-}
    {-show (Entry word dat) = unlines (word : show' 1 dat)-}
      {-where-}
        {-show' :: Int -> [(PartOfSpeech, Definition, Maybe SoundUrl)] -> [String]-}
        {-show' n ((pos,def,soundUrl):xs) =-}
            {-let soundUrlText = case soundUrl of-}
                                    {-Just soundUrl' -> " (" ++ soundUrl' ++ ")"-}
                                    {-Nothing -> ""-}
            {-in (show n ++ ". (" ++ pos ++ ") " ++ def ++ soundUrlText) : show' (n+1) xs-}
        {-show' _ [] = []-}

lookupWord :: String -> IO [Entry]
lookupWord word = either (const []) (entryInternalToEntries word . makeEntry) <$> getResponse word

{-
lookupWordDebug :: String -> IO (Either String Entry)
lookupWordDebug word = lookupWord' Left (Right . makeEntry word) word
-}

makeEntry :: Response -> EntryInternal
makeEntry response = flip execState initEntryInternal $ mapM processPrimary (responsePrimaries response)

processPrimary :: Primary -> State EntryInternal ()
processPrimary (Primary pentries terms _) = do
    mapM_ processPentry pentries
    mapM_ processPterm terms

processPentry :: PEntry -> State EntryInternal ()
processPentry (PEntry _ terms PEMeaning) = mapM_ processPentryTerm terms
processPentry _ = return ()

processPentryTerm :: Term -> State EntryInternal ()
processPentryTerm (Term _ _ def TText) = eiDefinitions %= (def:)
processPentryTerm _ = return ()

processPterm :: Term -> State EntryInternal ()
processPterm (Term (Just labels) _ _ TText) = processPtermLabels labels
processPterm (Term _ _ soundUrl TSound) = eiSoundUrl .= Just soundUrl
processPterm (Term _ _ phonetic TPhonetic) = eiPhonetic .= phonetic
processPterm _ = return ()

processPtermLabels :: [Label] -> State EntryInternal ()
processPtermLabels ((Label pos (Just "Part-of-speech")):_) = eiPartOfSpeech .= Just pos
processPtermLabels (_:xs) = processPtermLabels xs
processPtermLabels [] = return () -- No part of speech!

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
