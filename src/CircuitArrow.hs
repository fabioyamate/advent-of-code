{-# LANGUAGE Arrows #-}
module CircuitArrow where

import Control.Arrow ((&&&), (>>>), (***))

-- Arrow = (->)
-- Arrow b c => b -> c

-- arrow composition
-- (>>>) :: (Arrow a) => a b c -> a c d -> a b d

-- combine arrow
-- (&&&) :: (Arrow a) => a b c -> a b c' -> a b (c, c')

-- parallel arrow
-- (***) :: (Arrow a) => a b c -> a b' c' -> a (b, b') (c, c')

type Circuit = (->)

-- (Bool, Bool) -> Bool
nandGate :: Circuit (Bool, Bool) Bool
nandGate = \(x,y) -> not (x && y)

wire :: Circuit Bool Bool
wire = id

splittedWire :: Circuit Bool (Bool, Bool)
splittedWire = (wire &&& wire)

inverter :: Circuit Bool Bool
inverter = splittedWire >>> nandGate

parallelInverters :: Circuit (Bool, Bool) (Bool, Bool)
parallelInverters = inverter *** inverter

orGate :: Circuit (Bool, Bool) Bool
orGate = parallelInverters >>> nandGate

orGate' = proc (a, b) -> do
  m1 <- nandGate -< (a, a)
  m2 <- nandGate -< (b, b)
  nandGate -< (m1, m2)
