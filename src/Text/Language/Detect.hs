{-# LANGUAGE DeriveDataTypeable #-}

module Text.Language.Detect (detect,detectCode) where

import Text.Language.Internals
import Text.JSON.Generic 
import Prelude hiding ((.), (-))

-- http://ajax.googleapis.com/ajax/services/language/detect?v=1.0&q=Hello+World

{-
{"responseData"=>{"language"=>"en","isReliable"=>false,"confidence"=>0.114892714},
 "responseStatus"=>200,
 "responseDetails"=>nil}
-}

-- Datatypes
data RText = RText
  {
    language   :: String,
    isReliable :: Bool,
    confidence :: Double
  }
  deriving (Eq, Show, Data, Typeable)

data RGood = RGood
  {
    responseData :: RText
  }
  deriving (Eq, Show, Data, Typeable)
 

base_url :: String
base_url = "http://ajax.googleapis.com/ajax/services/language/detect"

detect_api :: String -> String
detect_api what = google_api base_url [("v", "1.0"), ("q", what)] 
    
-- | Returns the language code associated with the given text
detectCode :: String -> IO (Maybe String)
detectCode what = do
  r <- curl - detect_api what
  case r of
    Nothing -> return - Nothing
    Just x -> 
      let status = x.decodeJSON
      in
      if status.responseStatus == 200
        then do
          let rgood = x.decodeJSON
          return - Just - rgood.responseData.language
        else do
          return Nothing

-- | Returns a triple where the first component is the language code associated with
--   given text, the second is a boolean representing whether or not the detection interval 
--   believes the language code is reliable for the given text, and the third is a
--   numeric value between 0-1.0 that represents the confidence level in the language code
--   for the given text.
detect :: String -> IO (Maybe (String,Bool,Double))
detect what = do
  r <- curl - detect_api what
  case r of
    Nothing -> return - Nothing
    Just x -> 
      let status = x.decodeJSON
      in
      if status.responseStatus == 200
        then do
          let rgood = x.decodeJSON
          return - Just - (rgood.responseData.language,
                           rgood.responseData.isReliable,
                           rgood.responseData.confidence)
        else do
          return Nothing
