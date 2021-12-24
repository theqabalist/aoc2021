module Days.Day24.Instruction where

import Debug.Trace
import Control.Monad.Except
import Days.Day24.Machine hiding (read)
import qualified Days.Day24.Machine as M
import Prelude

data Operand = R Register | I Int
  deriving (Show)

data Instruction
  = Input Register
  | Add Register Operand
  | Multiply Register Operand
  | Equal Register Operand
  | Divide Register Operand
  | Modulo Register Operand
  deriving (Show)

data MachineContinuation = Waiting (Char -> Machine -> Except String Machine) | Continuing (Machine -> Except String Machine)

eval :: Operand -> Machine -> Int
eval (R reg) = M.read reg
eval (I v) = const v

genericOp :: (Int -> Int -> Except String Int) -> Register -> Operand -> MachineContinuation
genericOp f reg op = Continuing (\m -> flip (set reg) m <$> (f (M.read reg m) (eval op m)))

compile :: Instruction -> MachineContinuation
compile (Input reg) = Waiting (\c m -> pure $ set reg (read [c]) m)
compile (Add reg op) = genericOp (\x y -> pure $ x + y) reg op
compile (Multiply reg op) = genericOp (\x y -> pure $ x * y) reg op
compile (Divide reg op) = genericOp (\x y -> if y == 0 then throwError "Div0" else pure (floor (fromIntegral x / fromIntegral y :: Double))) reg op
compile (Modulo reg op) = genericOp (\x y -> if x < 0 || y <= 0 then throwError "Mod0" else pure (x `mod` y)) reg op
compile (Equal reg op) = genericOp (\x y -> pure $ if x == y then 1 else 0) reg op

runMachine :: [Instruction] -> String -> Except String Machine
runMachine instructions input = runMachine' (compile <$> instructions) input (Machine 0 0 0 0)
  where
    runMachine' :: [MachineContinuation] -> String -> Machine -> Except String Machine
    runMachine' ((Continuing f) : is) input m = traceShow m $ f m >>= runMachine' is input
    runMachine' ((Waiting f) : is) (c : cs) m = traceShow m $ f c m >>= runMachine' is cs
    runMachine' ((Waiting _) : _) [] _ = throwError "ran out of input"
    runMachine' [] _ m = pure m