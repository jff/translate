{-# LANGUAGE DeriveDataTypeable #-}

module Text.Language.Internals where

import Text.JSON.Generic
import Network.Curl
import qualified Data.List as L
import Prelude hiding ((.), (-))
import Network.URI (isAllowedInURI)
import qualified Codec.Binary.UTF8.String as Utf
import Numeric

-- This module factors out auxiliar and similar functions to the Text.Language modules

-- Datatypes
data RStatus = RStatus
  {
    responseStatus :: Integer
  }
  deriving (Eq, Show, Data, Typeable)

   
data RBad = RBad
  {
    responseDetails :: String
  }
  deriving (Eq, Show, Data, Typeable)


-- | Perform a request using Network.Curl
--
curl :: String -> IO (Maybe String)
curl x = do
  (r, s) <- curlGetString x []
  if r == CurlOK
    then return - Just s
    else return Nothing

-- | Constructs a string with the given arguments ready to be sent
--   to Google's APIs
google_api :: String -> [(String,String)] -> String
google_api base_url args = 
  let make_pair (x, y) = x ++ "=" ++ escape_uri y
  in
  base_url ++ "?" ++  args .map make_pair .join "&"
 


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



-- Google APIs encode text as ASCII, but it is first escaped with UTF-8.
-- Therefore, the |escapeURIString| function in Network.URI cannot be used. 
-- We redefine it here.

-- | Can be used to validate the URI sent to Google's API
--
escape_uri :: String -> String
escape_uri = escapeURIString isAllowedInURI

-- | Escapes a special character with UTF-8.
--
escapeURIChar :: (Char->Bool) -> Char -> String
escapeURIChar p c
    | p c       = [c]
    | otherwise = concatMap ('%':) $ map (flip showHex "") $ Utf.encode [c]

-- | Can be used to make a string valid for use in a URI.
--
escapeURIString
    :: (Char->Bool)     -- ^ a predicate which returns 'False'
                        --   if the character should be escaped
    -> String           -- ^ the string to process
    -> String           -- ^ the resulting URI string
escapeURIString p s = concatMap (escapeURIChar p) s
