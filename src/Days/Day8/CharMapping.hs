{-# LANGUAGE NamedFieldPuns #-}

module Days.Day8.CharMapping where

import Data.Map (Map, empty, insert, lookup)
import Prelude hiding (lookup)

data CharMapping = CharMapping
  { fromScrambled :: Map Char Char,
    toScrambled :: Map Char Char
  }
  deriving (Show)

mkMapping :: CharMapping
mkMapping = CharMapping empty empty

update :: Char -> Char -> CharMapping -> CharMapping
update scrambled deduced (CharMapping fromScrambled toScrambled) = CharMapping (insert scrambled deduced fromScrambled) (insert deduced scrambled toScrambled)

descramble :: Char -> CharMapping -> Maybe Char
descramble c CharMapping {fromScrambled} = lookup c fromScrambled

scramble :: Char -> CharMapping -> Maybe Char
scramble c CharMapping {toScrambled} = lookup c toScrambled