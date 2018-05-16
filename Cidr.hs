module Cidr
  ( Cidr(..)
  , parse
  ) where

import Control.Monad
import Data.Char
import Data.Word
import Text.ParserCombinators.ReadP

data Cidr = Cidr
  { netmask :: Maybe Int
  , address :: (Word8, Word8, Word8, Word8)
  }

parse :: String -> Maybe Cidr
parse s = do
  [(x, "")] <- pure (readP_to_S parser s)
  pure x

parser :: ReadP Cidr
parser = do
  a <- hunk <* char '.'
  b <- hunk <* char '.'
  c <- hunk <* char '.'
  d <- hunk
  ss <- look
  case ss of
    '/':_ -> do
      n <- char '/' *> net
      pure Cidr
        { netmask = Just n
        , address = (a, b, c, d)
        }
    _ ->
      pure Cidr
        { netmask = Nothing
        , address = (a, b, c, d)
        }

hunk :: ReadP Word8
hunk = do
  n <- read <$> munch1 isDigit
  guard ((n::Int) >= 0 && n <= 255)
  pure (fromIntegral n)

net :: ReadP Int
net = do
  n <- read <$> munch1 isDigit
  guard (n >= 0 && n <= 32)
  pure n
