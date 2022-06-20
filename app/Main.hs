module Main where

import Data.Char (toLower)
import Data.Foldable
import Data.List
import GHC.ExecutionStack (Location (functionName))
import Languages
import SongKeys
import System.Exit (exitSuccess)
import System.IO

main :: IO () --Here we are using haskell's most common monad -- the IO monad
-- Haskell provides do notation to use with monads, and while every monad
-- behaves slightly differently the do notation with the IO monad looks very
-- much like imperative code in that each line is executed in sequence
-- The first line is a simple instruction where we igore the resulting value
--
-- Something to pay attention to all the functions we call in the do block need
-- to return the same monad, in this case the IO monad
main = do
  putStrLn "Hello, Welcome to haskell translator!!"
  -- Here we actually want to use the value from the functions later on.
  -- We are calling the functions "chooseSourceLanguage" and "getSourceLanguage"
  -- They each return a "Language" (see Language.hs) wrapped in a IO monad (remeber
  -- everything in a do block needs to return the same monad)
  sourceLanguage <- chooseSourceLanguage
  targetLanguage <- chooseTargetLanguage
  -- Now we are passing control permineantly to the interactiveTranslate function
  -- haskell functions return the value of the last expresion, so "main" will return
  -- the result of interactiveTranslate. Notice that both functions return the same type
  -- "IO ()", which just means an IO monad that doesn't return any value
  interactiveTranslate sourceLanguage targetLanguage

chooseSourceLanguage :: IO Language
chooseSourceLanguage = do
  -- Notice that we are still in the IO monad. That is because we need to print to
  -- the screen, and get input from the user, both of which you can only do from
  -- inside the IO monad. However unlike the "main" fuction, we are actually returning
  -- something here: the Language value
  putStrLn "Please type the name of the lanuage that you want to translate from"
  putStr "Available Language Choices: "
  -- Operator precedence is of key importance in haskell. Here we want to apply
  -- putStrLn to the resulting string, not to the intercalate function, therefore we
  -- use the dollar signs. The dollar signs are an alternative to putting braces from
  -- where the dollar sign starts to the end of the line so this line with braces would
  -- look like "putStrLn (intercalate ", " (languageName <$> languages))"
  -- The second thing to pay attention to is "<$>": It is fmap, and as it is acting
  -- over a list it is equivalent to map functions that other languages have for lists
  -- (languageName <$> languages) is the same as (fmap languageName languages), and
  -- applies the function languageName to every element in the list languages, with the
  -- result being a new list of the return values.
  putStrLn $ intercalate ", " $ languageName <$> languages
  -- We are going over to another function, because we want to run it in a loop, or
  -- recursively. That allows us to ask for input again if we were not able to understand
  -- what the user wrote
  chooseLanguage

-- Everything in this function besides the text written is identical to the one above
chooseTargetLanguage :: IO Language
chooseTargetLanguage = do
  putStrLn "Please type the name of the lanuage that you want to translate to"
  putStr "Available Language Choices: "
  putStrLn $ intercalate ", " (languageName <$> languages)
  chooseLanguage

chooseLanguage :: IO Language
chooseLanguage = do
  language <- getLine
  case convertToLanguage language of
    Nothing -> do      
      putStrLn "What you entered didn't match any of our language options, please try again"
      chooseLanguage
    Just x -> pure x
  where
    convertToLanguage :: String -> Maybe Language
    convertToLanguage input = find (matcher input) languages
    matcher :: String -> Language -> Bool
    matcher input language = (toLower <$> input) == (toLower <$> languageName language)

interactiveTranslate :: Language -> Language -> IO ()
interactiveTranslate sourceLanguage targetLanguage = do
  putStrLn ("You are translating from " ++ languageName sourceLanguage ++ " to " ++ languageName targetLanguage)
  putStrLn "To exit type :exit and press enter, to translate enter the sentence or word you want to translate and press enter"
  translationLoop
  where
    translationLoop :: IO ()
    translationLoop = do
      putStr (languageName sourceLanguage ++ " -> " ++ languageName targetLanguage ++ " > ")
      hFlush stdout
      toTranslate <- getLine
      case toTranslate of
        ":exit" -> putStrLn "Thank you!!!" 
                   exitSuccess
        ":change" -> main
        "" -> translationLoop
        _ -> do
          translatedString <- translate sourceLanguage targetLanguage toTranslate
          putStrLn translatedString
          translationLoop
