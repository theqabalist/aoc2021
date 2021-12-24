module Days.Day24.Parsing where

import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text (Parser, char, decimal, endOfInput, endOfLine, parseOnly, signed, string)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Text (Text)
import Days.Day24.Instruction
import Days.Day24.Machine
import Lib (Parseable (parse))
import Prelude

newtype Day24Input = Day24Input [Instruction]
  deriving (Show)

register :: Parser Register
register =
  char 'x' $> X
    <|> char 'y' $> Y
    <|> char 'z' $> Z
    <|> char 'w' $> W

input :: Parser Instruction
input = Input <$> (string "inp " *> register)

operand :: Parser Operand
operand = R <$> register <|> (I <$> signed decimal)

operandInstruction :: Text -> (Register -> Operand -> Instruction) -> Parser Instruction
operandInstruction slug cons = cons <$> (string slug *> char ' ' *> register <* char ' ') <*> operand

add :: Parser Instruction
add = operandInstruction "add" Add

multiply :: Parser Instruction
multiply = operandInstruction "mul" Multiply

divide :: Parser Instruction
divide = operandInstruction "div" Divide

modulo :: Parser Instruction
modulo = operandInstruction "mod" Modulo

equal :: Parser Instruction
equal = operandInstruction "eql" Equal

instruction :: Parser Instruction
instruction = input <|> add <|> multiply <|> divide <|> divide <|> modulo <|> equal

day24Input :: Parser Day24Input
day24Input = Day24Input <$> many (instruction <* endOfLine) <* endOfInput

instance Parseable Day24Input where
  parse = fromRight (error "invalid input") . parseOnly day24Input