module Data.Martix

import Data.Vect
import Data.String

public export
MartixItem : Type
MartixItem = Int

public export
Martix : (rows : Nat) -> (cols : Nat) -> Type
Martix rows cols = Vect rows (Vect cols MartixItem)

public export
zeros : (rows : Nat) -> (cols : Nat) -> (Martix rows cols)
zeros rows cols = replicate rows $ replicate cols 0

public export
ones : (rows : Nat) -> (cols : Nat) -> (Martix rows cols)
ones rows cols = replicate rows $ replicate cols 1

public export
updateAt : (row : Fin rows) -> (col : Fin cols) -> (updateFun: MartixItem -> MartixItem) -> (Martix rows cols) -> (Martix rows cols)
updateAt row col = updateAt row . updateAt col 

public export
replaceAt : (row : Fin rows) -> (col : Fin cols) -> (ele: MartixItem) -> (Martix rows cols) -> (Martix rows cols)
replaceAt row col ele = updateAt row col $ const ele

public export
eye : (rows : Nat) -> (cols : Nat) -> Martix rows cols
eye rows cols  = go (rangeFromTo 0 $ min rows cols) (zeros rows cols)
  where
    go : List Nat -> Martix rows cols -> Martix rows cols
    go [] martix = martix
    go (x :: xs) martix = case (natToFin x rows, natToFin x cols) of
                        (Just fx, Just fy) => replaceAt fx fy 1 $ go xs martix 
                        (_, _) => go xs martix

public export
identity : (rows : Nat)  -> Martix rows rows
identity rows = eye rows rows


public export
repmat : Vect n MartixItem -> (rows : Nat) -> (cols : Nat) -> Martix rows (cols * n)
repmat vect rows cols = replicate rows $ concat $  Data.Vect.replicate cols vect

public export
transpose : {rows: Nat} -> {cols: Nat} ->  (martix : Martix rows cols)  -> Martix cols rows
transpose = Data.Vect.transpose

public export
zipWith : (MartixItem -> MartixItem -> MartixItem) -> Martix rows cols -> Martix rows cols -> Martix rows cols
zipWith f left right = Data.Zippable.zipWith (zipWith f) left right
 
public export
(+) :  (martix : Martix rows cols) ->  (martix : Martix rows cols)  -> Martix rows cols
(+) = Data.Martix.zipWith (+)

public export
(-) :  (martix : Martix rows cols) ->  (martix : Martix rows cols)  -> Martix rows cols
(-) = Data.Martix.zipWith (-)

public export
prettyShow : (martix : Martix rows cols) -> String
prettyShow martix = joinBy "\n" $ map show $ toList martix

