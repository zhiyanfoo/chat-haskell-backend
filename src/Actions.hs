{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}

module Actions (jsonFile, Action, getJson) where

import Data.Aeson
  ( FromJSON
  , Value(..)
  , (.:)
  , (.=)
  , decode
  , encode
  , object
  , parseJSON
  , withArray
  , withObject
  )
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Vector as V
import GHC.Exts (fromList)

jsonFile :: FilePath
jsonFile = "test.json"

getJson :: IO B.ByteString
getJson = B.readFile jsonFile

main = do
  putStrLn "ASD"
  print $ encode val
  print $ encode val2
  print $ parseMaybe parseArray2 =<< decode "[{\"a\":\"he\",\"b\":true}]"
  file <- getJson
  print $ (decode file :: Maybe Action)
  BC.putStrLn file

val :: Value
val =
  Object $
  fromList
    [("numbers", Array $ fromList [Number 1, Number 2]), ("hello", Bool True)]

val2 :: Value
val2 = object ["bol" .= True, "numb" .= ([1, 2, 3] :: [Int])]

parseTuple :: Value -> Parser (String, Bool)
parseTuple (Object obj) = do
  let mbFieldA = HM.lookup "a" obj
  fieldA <-
    case mbFieldA of
      Just x -> return x
      Nothing -> fail "no field 'a'"
  a <-
    case fieldA of
      String x -> return (T.unpack x)
      _ -> fail "expected a string"
  b <-
    case HM.lookup "b" obj of
      Just (Bool x) -> return x
      Just _ -> fail "expected a boolean"
      Nothing -> fail "no field 'b'"
  return (a, b)

parseArray :: Value -> Parser [(String, Bool)]
parseArray (Array arr) = mapM parseTuple (V.toList arr)
parseArray _ = fail "expected an array"

parseArray2 :: Value -> Parser [(String, Bool)]
parseArray2 =
  withArray "array of tuples" $ \arr -> mapM parseTuple (V.toList arr)

parseTuple2 :: Value -> Parser (String, Bool)
parseTuple2 =
  withObject "tuple" $ \obj -> do
    a <-
      case HM.lookup "a" obj of
        Just x -> parseJSON x
        Nothing -> fail "no field a"
    b <-
      case HM.lookup "b" obj of
        Just x -> parseJSON x
        Nothing -> fail "no field b"
    return (a, b)

parseTuple3 :: Value -> Parser (String, Bool)
parseTuple3 =
  withObject "tuple" $ \o -> do
    a <- o .: "a"
    b <- o .: "b"
    return (a, b)

data Action
  = AddMessage { author :: Text
               , message :: Text }
  | AddUser { username :: Text }
  deriving (Show)

instance FromJSON Action where
  parseJSON =
    withObject "action" $ \o -> do
      kind <- o .: "type"
      case kind of
        "ADD_MESSAGE" -> AddMessage <$> o .: "author" <*> o .: "message"
        "ADD_USER" -> AddUser <$> o .: "username"
        _ -> fail ("unknown kind: " ++ kind)
