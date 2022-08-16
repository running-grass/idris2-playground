module Data.Martix

import Data.Vect
import Data.String

-- public export
-- MartixItem : Type
-- MartixItem = Int 


export
data Martix : Nat -> Nat -> Type -> Type where
  MkMartix : Vect rows (Vect cols a) -> Martix rows cols a



public export
prettyShow : Show n => (martix : Martix rows cols n) -> String
prettyShow (MkMartix martix) = joinBy "\n" $ map show $ toList martix

public export
Eq a => Eq (Martix rows cols a) where
  (MkMartix lv) == (MkMartix rv) = lv == rv

public export
Show a => Show (Martix rows cols a) where
  show = prettyShow

public export
Functor (Martix rows cols) where
  map f (MkMartix ma) = MkMartix $ map (map f) ma

public export
Zippable (Martix rows cols) where
  zipWith f (MkMartix lv) (MkMartix rv)  = MkMartix $ zipWith (zipWith f) lv rv
  unzipWith f v  = let ma = (map f v) in (fst <$> ma,snd <$> ma)

  zipWith3 f (MkMartix a) (MkMartix b)  (MkMartix c) = MkMartix $ zipWith3 (zipWith3 f) a b c
  unzipWith3 f v  = let ma = (map f v) in (fst <$> ma, fst . snd <$> ma, snd . snd <$> ma)


public export
fromVects : Vect rows (Vect cols a) -> Martix rows cols a
fromVects = MkMartix



public export
zeros : (rows : Nat) -> (cols : Nat) -> (Martix rows cols Int)
zeros rows cols = MkMartix $ replicate rows $ replicate cols 0

public export
ones : (rows : Nat) -> (cols : Nat) -> (Martix rows cols Int)
ones rows cols = MkMartix $ replicate rows $ replicate cols 1

public export
updateAt : (row : Fin rows) -> (col : Fin cols) -> (updateFun: a -> a) -> (Martix rows cols a) -> Martix rows cols a
updateAt row col f (MkMartix vects) = MkMartix $ updateAt row (updateAt col f) vects

public export
replaceAt : (row : Fin rows) -> (col : Fin cols) -> (ele) -> (Martix rows cols ele) -> (Martix rows cols ele)
replaceAt row col ele = updateAt row col $ const ele

-- a : Martix rows cols String -> Martix rows cols String
public export
eye : (rows : Nat) -> (cols : Nat) -> Martix rows cols Int
eye rows cols  = go (rangeFromTo 0 $ min rows cols) (zeros rows cols)
  where
    go : List Nat -> Martix rows cols Int -> Martix rows cols Int
    go [] martix = martix
    go (x :: xs) martix = case (natToFin x rows, natToFin x cols) of
                        (Just fx, Just fy) => replaceAt fx fy 1 $ go xs martix 
                        (_, _) => go xs martix

public export
identity : (rows : Nat)  -> Martix rows rows Int
identity rows = eye rows rows


public export
repmat : Num ele => Vect n ele -> (rows : Nat) -> (cols : Nat) -> Martix rows (cols * n) ele
repmat vect rows cols = MkMartix $ replicate rows $ concat $  Data.Vect.replicate cols vect

public export
transpose : {rows: Nat} -> {cols: Nat} ->  (martix : Martix rows cols n)  -> Martix cols rows n
transpose (MkMartix vects) = MkMartix $ Data.Vect.transpose vects

-- public export
-- zipWith : Num n => {rows n: _} -> { cols n: _ } -> (Martix rows cols n) -> (Martix rows cols n) -> (Martix rows cols n)
-- zipWith  (MkMartix lv) (MkMartix rv) = MkMartix rv

 -- zipWith f (MkMartix leftVects) (MkMartix rightVects) = MkMartix $ Data.Zippable.zipWith (zipWith f) leftVects rightVects

public export
(+) : Num n => (martix : Martix rows cols n) ->  (martix : Martix rows cols n)  -> Martix rows cols n
(+) = zipWith (+)
