module Data.Martix

import Data.Vect

public export
Martix : (rows : Nat) -> (cols : Nat) -> Type
Martix rows cols = Vect rows (Vect cols Int)

public export
zeros : (rows : Nat) -> (cols : Nat) -> (Martix rows cols)
zeros rows cols = replicate rows $ replicate cols 0

public export
ones : (rows : Nat) -> (cols : Nat) -> (Martix rows cols)
ones rows cols = replicate rows $ replicate cols 1
