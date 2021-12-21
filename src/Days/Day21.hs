module Days.Day21 where

import Control.Monad.State
  ( MonadState (get),
    State,
    evalState,
    modify,
  )
import Data.Attoparsec.Text
  ( Parser,
    decimal,
    endOfInput,
    endOfLine,
    parseOnly,
    string,
  )
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Map (Map, empty, insert, lookup)
import Lib (Parseable (parse))
import Prelude hiding (lookup)

newtype Day21Input = Day21Input {day21 :: (Int, Int)}
  deriving (Show)

day21Input :: Parser (Int, Int)
day21Input = (,) <$> (string "Player 1 starting position: " *> decimal <* endOfLine) <*> (string "Player 2 starting position: " *> decimal) <* endOfInput

instance Parseable Day21Input where
  parse = Day21Input . fromRight (error "invalid input") . parseOnly day21Input

data PlayState = PlayState
  { player1 :: (Int, Int),
    player2 :: (Int, Int),
    currentPlayer :: Int,
    die :: Int,
    rolls :: Int,
    turns :: Int
  }
  deriving (Show, Eq, Ord)

mkPlayState :: (Int, Int) -> PlayState
mkPlayState (p1, p2) =
  PlayState
    { player1 = (p1, 0),
      player2 = (p2, 0),
      currentPlayer = 0,
      die = 1,
      rolls = 0,
      turns = 0
    }

ply :: PlayState -> PlayState
ply (PlayState (p1pos, p1score) p2 0 die rolls turns) =
  let pos = ((p1pos - 1) + sum [die .. die + 2]) `mod` 10 + 1
      score = p1score + pos
      newDie = (die - 1 + 3) `mod` 100 + 1
   in PlayState (pos, score) p2 1 newDie (rolls + 3) (turns + 1)
ply (PlayState p1 (p2pos, p2score) 1 die rolls turns) =
  let pos = ((p2pos - 1) + sum [die .. die + 2]) `mod` 10 + 1
      score = p2score + pos
      newDie = (die - 1 + 3) `mod` 100 + 1
   in PlayState p1 (pos, score) 0 newDie (rolls + 3) (turns + 1)
ply p@(PlayState _ _ n _ _ _) = ply (p {currentPlayer = n `mod` 2})

ply' :: [Int] -> PlayState -> PlayState
ply' dice (PlayState (p1pos, p1score) p2 0 _ rolls turns) =
  let pos = ((p1pos - 1) + sum dice) `mod` 10 + 1
      score = p1score + pos
   in PlayState (pos, score) p2 1 (-1) (rolls + 3) (turns + 1)
ply' dice (PlayState p1 (p2pos, p2score) 1 _ rolls turns) =
  let pos = ((p2pos - 1) + sum dice) `mod` 10 + 1
      score = p2score + pos
   in PlayState p1 (pos, score) 0 (-1) (rolls + 3) (turns + 1)
ply' dice p@(PlayState _ _ n _ _ _) = ply' dice (p {currentPlayer = n `mod` 2})

complete :: Int -> PlayState -> Bool
complete n (PlayState (_, p1score) (_, p2score) _ _ _ _) = p1score >= n || p2score >= n

deterministic :: (Int -> PlayState -> Bool) -> PlayState -> Bool
deterministic f = f 1000

quantum :: (Int -> PlayState -> Bool) -> PlayState -> Bool
quantum f = f 21

data Player = One | Two
  deriving (Show, Eq)

isOne :: Player -> Bool
isOne One = True
isOne _ = False

other :: Player -> Player
other One = Two
other Two = One

winner :: PlayState -> Player
winner (PlayState (_, p1score) (_, p2score) _ _ _ _) = if p1score > p2score then One else Two

partOne :: Day21Input -> Int
partOne = finalize . head . dropWhile (not . deterministic complete) . iterate ply . mkPlayState . day21
  where
    finalize (PlayState (_, p1score) (_, p2score) _ _ rolls _) = rolls * min p1score p2score

type Cache = Map PlayState ((Player, Int), (Player, Int))

tripletToList :: forall a. (a, a, a) -> [a]
tripletToList (one, two, three) = [one, two, three]

searchWinner :: PlayState -> State Cache ((Player, Int), (Player, Int))
searchWinner state | quantum complete state = do
  let result = if winner state == One then ((One, 1), (Two, 0)) else ((One, 0), (Two, 1))
  modify (insert state result) $> result
searchWinner state = do
  cache <- get
  let found = lookup state cache
  case found of
    Nothing -> do
      more <- traverse searchWinner $ flip ply' state . tripletToList <$> ((,,) <$> [1 .. 3] <*> [1 .. 3] <*> [1 .. 3])
      let oneWins = sum $ snd . fst <$> more
      let twoWins = sum $ snd . snd <$> more
      let result = ((One, oneWins), (Two, twoWins))
      modify (insert state result) $> result
    Just p -> pure p

partTwo :: Day21Input -> Int
partTwo (Day21Input input) =
  let state = mkPlayState input
      ((_, p1score), (_, p2score)) = evalState (searchWinner state) empty
   in max p1score p2score