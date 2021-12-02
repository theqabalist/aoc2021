{-# LANGUAGE NamedFieldPuns #-}

module Days.Day2.Movement.Interp where

import Days.Day2.Movement
import Days.Day2.Position

compile :: Movement -> Position -> Position
compile (Forward units) p@Position {x} = p {x = x + units}
compile (Up units) p@Position {z} = p {z = z - units}
compile (Down units) p@Position {z} = p {z = z + units}
compile Stay p = p