{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson (Value(..), (.=), encode, object, decode)
import GHC.Exts (fromList)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Vector as V

main = do
  putStrLn "ASD"
  print $ encode val
  print $ encode val2
  print $ parseMaybe parseTuple =<< decode "{\"a\":\"he\",\"b\":true}" 

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
  fieldA <- case mbFieldA of
    Just x -> return x
    Nothing -> fail "no field 'a'"

  a <- case fieldA of
          String x -> return (T.unpack x)
          _ -> fail "expected a string"

  b <- case HM.lookup "b" obj of
         Just (Bool x) -> return x
         Just _ -> fail "expected a boolean"
         Nothing -> fail "no field 'b'"

  return (a, b)

parseArray :: Value -> Parser [(String, Bool)]
parseArray (Array arr) = mapM parseTuple (V.toList arr)
parseArray _ = fail "expected an array"
