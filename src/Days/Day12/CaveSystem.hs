module Days.Day12.CaveSystem where

import Data.Foldable (foldl')
import qualified Data.HashSet as HS
import Data.Map (Map, alter, empty, findWithDefault, fromList, toList, unionWith)
import Data.Maybe (isJust)
import Data.Text (lines, splitOn)
import Days.Day12.Cave
import Lib (Parseable (parse))
import Prelude hiding (lines, lookup)

newtype CaveSystem = CaveSystem (Map Cave (HS.HashSet Cave))
  deriving (Show)

mkCaveSystem :: CaveSystem
mkCaveSystem = CaveSystem empty

insertAssociation :: (Cave, Cave) -> CaveSystem -> CaveSystem
insertAssociation (c1, c2) (CaveSystem theMap) =
  let cave1 = findWithDefault HS.empty c1 theMap
      cave2 = findWithDefault HS.empty c2 theMap
   in CaveSystem $ unionWith HS.union (fromList [(c1, HS.insert c2 cave1), (c2, HS.insert c1 cave2)]) theMap

fromBidirectionalAssociations :: [(Cave, Cave)] -> CaveSystem
fromBidirectionalAssociations = foldl' (flip insertAssociation) mkCaveSystem

connections :: Cave -> CaveSystem -> HS.HashSet Cave
connections cave (CaveSystem theMap) = findWithDefault HS.empty cave theMap

instance Parseable CaveSystem where
  parse = fromBidirectionalAssociations . fmap ((\(a : b : _) -> (parse a, parse b)) . splitOn "-") . lines

type Path = [Cave]

exploreGeneral :: ((Path, Cave) -> Bool) -> CaveSystem -> [Path]
exploreGeneral filtration cave = reverse <$> explore' [Start] cave
  where
    explore' :: Path -> CaveSystem -> [Path]
    explore' p _ | head p == End = [p]
    explore' p@(curr : _) c =
      let cxs = HS.filter (validTransition curr) $ connections curr c
          nexts = HS.filter (filtration . (p,)) cxs
       in HS.toList nexts >>= (\next -> explore' (next : p) c)
    explore' _ _ = undefined

explore1 :: CaveSystem -> [Path]
explore1 = exploreGeneral (not . (\(path, cave) -> elem cave path && isSmall cave))

hasSmallDoublet :: Path -> Bool
hasSmallDoublet p =
  let counts = foldl' (flip (alter (\x -> if isJust x then (+ 1) <$> x else pure 1))) empty p
   in any (\(k, v) -> isSmall k && v == (2 :: Int)) (toList counts)

explore2 :: CaveSystem -> [Path]
explore2 = exploreGeneral (\(path, cave) -> not (hasSmallDoublet path) || not (elem cave path && isSmall cave))