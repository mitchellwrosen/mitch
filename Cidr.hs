module Cidr
  ( Cidr(..)
  , parseCidr
  ) where

import Mitchell.Prelude hiding (optional)

import Parser.Text
import Read.Partial (read)

data Cidr = Cidr
  { netmask :: Maybe Int
  , address :: (Word8, Word8, Word8, Word8)
  }

type Parser
  = Parsec () [Char]

parseCidr :: [Char] -> Maybe Cidr
parseCidr = do
  parseMaybe parser

parser :: Parser Cidr
parser = do
  a <- hunk <* char '.'
  b <- hunk <* char '.'
  c <- hunk <* char '.'
  d <- hunk
  n <- optional (char '/' *> net)
  pure Cidr
    { netmask = n
    , address = (a, b, c, d)
    }

hunk :: Parser Word8
hunk = do
  n <- read <$> some digitChar
  guard ((n::Int) >= 0 && n <= 255)
  pure (fromIntegral n)

net :: Parser Int
net = do
  n <- read <$> some digitChar
  guard (n >= 0 && n <= 32)
  pure n
