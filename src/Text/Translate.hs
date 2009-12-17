{-# LANGUAGE DeriveDataTypeable #-}

module Text.Translate (translate) where

import Text.JSON.Generic
import Network.Curl
import qualified Data.List as L
import Prelude hiding ((.), (-))
import Network.URI

-- http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&langpair=en|de&q=Hello+World

{-
{"responseData"=>{"translatedText"=>"Hallo Welt"},
 "responseStatus"=>200,
 "responseDetails"=>nil}
-}

curl :: String -> IO (Maybe String)
curl x = do
  (r, s) <- curlGetString x []
  if r == CurlOK
    then return - Just s
    else return Nothing

base_url :: String
base_url = "http://ajax.googleapis.com/ajax/services/language/translate"

trans_api :: String -> String -> String -> String
trans_api from to what = 
  let make_pair (x, y) = x ++ "=" ++ escape_uri y
  in
  base_url ++ "?" ++  [("v", "1.0"), ("langpair", from ++ "|" ++ to), ("q", what)] .map make_pair .join "&"

data RStatus = RStatus
  {
    responseStatus :: Integer
  }
  deriving (Eq, Show, Data, Typeable)

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
    
data RBad = RBad
  {
    responseDetails :: String
  }
  deriving (Eq, Show, Data, Typeable)

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


-- bolerplate


-- base DSL
{-# INLINE (.) #-}
(.) :: a -> (a -> b) -> b
a . f = f a
infixl 9 .

{-# INLINE (-) #-}
(-) :: (a -> b) -> a -> b
f - x =  f x
infixr 0 - 

join :: [a] -> [[a]] -> [a]
join = L.intercalate

escape_uri :: String -> String
escape_uri = escapeURIString isAllowedInURI