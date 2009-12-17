module Codec.SnowWhite (pack, unpack) where

import Data.Binary
import Numeric
import MPS.Env hiding (encode, decode)
import Prelude ()
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char

c2w, w2c :: Char -> Char
c2w '0' = ' '
c2w '1' = '\t'
c2w _   = error "not 0 or 1"

w2c ' '  = '0'
w2c '\t' = '1'
w2c _    = error "not 0 or 1"


b2s, pack :: (Binary a) => a -> String
b2s = encode > B.unpack > map (ord > (base 2) > rjust 8 '0' > map c2w) > concat

base :: Int -> Int -> String
base p n = showIntAtBase p intToDigit n ""

from_base :: Int -> String -> Int
from_base p = readInt p (const True) digitToInt > first > fst

s2b, unpack :: (Binary a) => String -> a
s2b = select (belongs_to " \t") > in_group_of 8 > map (map w2c > from_base 2 > chr) > B.pack > decode


pack = b2s
unpack = s2b