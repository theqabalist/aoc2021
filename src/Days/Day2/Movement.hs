{-# LANGUAGE OverloadedStrings #-}

module Days.Day2.Movement where

import Control.Applicative (some, (<|>))
import Data.Attoparsec.Text (Parser, endOfInput, endOfLine, parseOnly, skip, string)
import Data.Char (isSpace)
import Data.Text (Text)
import Days.Common.Parsing (int)
import Prelude (Either, Int, String, (*>), (<$>), (<*))

data Movement = Forward Int | Up Int | Down Int | Stay

intInstruction :: Text -> Parser Int
intInstruction ins = string ins *> skip isSpace *> int <* endOfLine

parseForward :: Parser Movement
parseForward = Forward <$> intInstruction "forward"

parseUp :: Parser Movement
parseUp = Up <$> intInstruction "up"

parseDown :: Parser Movement
parseDown = Down <$> intInstruction "down"

instruction :: Parser Movement
instruction = parseForward <|> parseUp <|> parseDown

instructions :: Parser [Movement]
instructions = some instruction

parseMovements :: Text -> Either String [Movement]
parseMovements = parseOnly (instructions <* endOfInput)