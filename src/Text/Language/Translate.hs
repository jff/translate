{-# LANGUAGE DeriveDataTypeable #-}

module Text.Language.Translate (translate) where

import Text.Language.Internals

import Text.JSON.Generic
import Prelude hiding ((.), (-))

-- http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&langpair=en|de&q=Hello+World

{-
{"responseData"=>{"translatedText"=>"Hallo Welt"},
 "responseStatus"=>200,
 "responseDetails"=>nil}
-}

-- Datatypes
data RText = RText
  {
    translatedText :: String
  }
  deriving (Eq, Show, Data, Typeable)

data RGood = RGood
  {
    responseData :: RText
  }
  deriving (Eq, Show, Data, Typeable)
 

base_url :: String
base_url = "http://ajax.googleapis.com/ajax/services/language/translate"

trans_api :: String -> String -> String -> String
trans_api from to what = google_api base_url [("v", "1.0"), ("langpair", from ++ "|" ++ to), ("q", what)]

translate :: String -> String -> String -> IO (Maybe String)
translate from to what = do
  r <- curl - trans_api from to what
  case r of
    Nothing -> return - Nothing
    Just x -> 
      let status = x.decodeJSON
      in
      if status.responseStatus == 200
        then do
          let rgood = x.decodeJSON
          return - Just - rgood.responseData.translatedText
        else do
          return Nothing
