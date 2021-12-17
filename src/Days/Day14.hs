module Days.Day14 where

import Control.Monad.State (State, evalState, get, put)
import Data.Foldable (foldl')
import Data.Map (Map, adjust, empty, findWithDefault, insert, lookup, singleton, toList, unionWith)
import Data.Maybe (fromMaybe)
import Days.Day14.Day14Input (Day14Input (..))
import Prelude hiding (lookup, maximum, minimum, rem)

frequency :: forall a. Ord a => [a] -> Map a Int
frequency =
  foldl'
    ( \m v ->
        let count = findWithDefault 0 v m
         in insert v (count + 1) m
    )
    empty

freqMinMax :: forall a. Map a Int -> (Int, Int)
freqMinMax m = minmax' (toList m)
  where
    minmax' :: [(a, Int)] -> (Int, Int)
    minmax' (first : rest) =
      let minimum = foldr (\x mn -> min mn (snd x)) (snd first) rest
          maximum = foldr (\x mx -> max mx (snd x)) (snd first) rest
       in (maximum, minimum)
    minmax' [] = error "cannot minmax an empty map"

countDescend :: Map String Char -> Int -> String -> State (Map (String, Int) (Map Char Int)) (Map Char Int)
countDescend _ depth input | depth == 0 = do
  cache <- get
  let updated = fromMaybe (frequency input) $ lookup (input, depth) cache
  put (insert (input, depth) updated cache)
  pure updated
countDescend mapping depth input@(first : next : rest) = do
  cache <- get
  let insertion = fromMaybe (error ("string '" <> [first, next] <> "' not found in lookup")) $ lookup [first, next] mapping
  leftCount <- maybe (countDescend mapping (depth - 1) [first, insertion]) pure (lookup ([first, insertion], depth - 1) cache)
  rightCount <- get >>= (maybe (countDescend mapping (depth - 1) [insertion, next]) pure . lookup ([insertion, next], depth - 1))
  let noDoublesLeft = adjust (subtract 1) insertion leftCount
  let noDoublesRight = adjust (subtract 1) next rightCount
  updated <- (\rem -> foldr (unionWith (+)) empty [noDoublesLeft, noDoublesRight, rem]) <$> countDescend mapping depth (next : rest)
  get >>= put . insert (input, depth) updated
  pure updated
countDescend _ _ [c] = pure (singleton c 1)
countDescend _ _ [] = pure empty

partOne :: Day14Input -> Int
partOne (Day14Input (tmpl, mapping)) = uncurry (-) . freqMinMax $ evalState (countDescend mapping 10 tmpl) empty

partTwo :: Day14Input -> Int
partTwo (Day14Input (tmpl, mapping)) = uncurry (-) . freqMinMax $ evalState (countDescend mapping 40 tmpl) empty