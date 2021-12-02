{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Days.Day2.Movement where

import Control.Applicative (many, some, (<|>))
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Attoparsec.Text (Parser, Result, decimal, digit, endOfInput, endOfLine, parseOnly, skip, skipMany1, space, string)
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Text (Text, pack)
import Days.Day2.Position (Position (Position), x, z)

data Movement = Forward Int | Up Int | Down Int | Stay

int :: Parser Int
int = read <$> some digit

intInstruction :: Text -> Parser Int
intInstruction instruction = string instruction *> skip isSpace *> int <* endOfLine

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