{-# LANGUAGE NamedFieldPuns #-}

module Days.Day2.Movement.Interp2 where

import Days.Day2.Movement (Movement (..))
import Days.Day2.Position (Position (..))
import Prelude ((*), (+), (-))

compile :: Movement -> Position -> Position
compile (Forward units) p@Position {x, z, aim} = p {x = x + units, z = z + aim * units}
compile (Up units) p@Position {aim} = p {aim = aim - units}
compile (Down units) p@Position {aim} = p {aim = aim + units}
compile Stay p = p