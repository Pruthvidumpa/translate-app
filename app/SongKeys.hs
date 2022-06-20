{-# LANGUAGE DeriveGeneric #-}

-- This is the file where we send an api request for translation and parse the result
-- I selected the SongKeys translator for their easy API and the fact that I wouldn't
-- need to get a specific API key for it. See https://github.com/Songkeys/Translateer and
-- https://t.song.work/
module SongKeys where

-- SongKeys API returns JSON that I will need the Aeson library to decode
import Data.Aeson (FromJSON)
import GHC.Generics
import Languages (Language (languageCode))
import Network.HTTP.Simple

-- To decode the json data, I first need to create a haskell data type that has the
-- same fields. After I do that I can use "instance FromJSON" to have the work automatically
-- to decode json.
data Translation = Translation
  { result :: String,
    pronunciation :: Maybe String,
    from :: ISO
  }
  deriving (Generic, Show)

data ISO = ISO
  {iso :: String}
  deriving (Generic, Show)

instance FromJSON ISO

instance FromJSON Translation

-- the translate function still needs to use the IO monad, because httpRequest (and parse
-- request) need IO to function. The formmer to throw errors, and the latter to access
-- the network interface
translate :: Language -> Language -> String -> IO String
translate sourceLanguage targetLanguage text = do
  -- parseRequest takes a string and tries to turn it into a request. If it fails it
  -- throws an errror
  request <- parseRequest ("https://t.song.work/api?text=" ++ text ++ "&from=" ++ languageCode sourceLanguage ++ "&to=" ++ languageCode targetLanguage ++ "&lite=true")
  -- I make the request with httpJSON which attempts to make the request and returns a
  -- response with the responseBody being my automatically deconverted json object
  -- (in this case the Translation data type). But before I access, it I need to apply
  -- the getter method "getResponseBody" to it. And then because I only want the result
  -- I also apply the "result" getter method on the returned Translation value.
  -- The reason why I am using fmap here is because everything is encapsulated in the IO
  -- monad, and we don't want to alter the monad, but just the value stored inside it.
  result . getResponseBody <$> httpJSON request
