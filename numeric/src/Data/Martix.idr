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
Eq a => Eq (Martix rows cols a) where
  (MkMartix lv) == (MkMartix rv) = lv == rv

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
toVects : Martix rows cols a -> Vect rows (Vect cols a) 
toVects (MkMartix v) = v


public export
prettyShow : Show n => (martix : Martix rows cols n) -> String
prettyShow (MkMartix martix) = joinBy "\n" $ map show $ toList martix

public export
Show a => Show (Martix rows cols a) where
  show = prettyShow

private 
dotProduct : Num a => Vect n a -> Vect n a -> a
dotProduct x y = sum $ zipWith (*) x y

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


-- public export
-- zipWith : Num n => {rows n: _} -> { cols n: _ } -> (Martix rows cols n) -> (Martix rows cols n) -> (Martix rows cols n)
-- zipWith  (MkMartix lv) (MkMartix rv) = MkMartix rv

 -- zipWith f (MkMartix leftVects) (MkMartix rightVects) = MkMartix $ Data.Zippable.zipWith (zipWith f) leftVects rightVects

public export
(+) : Num n => (martix1 : Martix rows cols n) ->  (martix2 : Martix rows cols n)  -> Martix rows cols n
(+) = zipWith (+)

public export
(-) : Neg n => (martix1 : Martix rows cols n) ->  (martix2 : Martix rows cols n)  -> Martix rows cols n
(-) = zipWith (-)

public export
scalarMulti : Num a => a -> (martix1 : Martix rows cols a) -> Martix rows cols a
scalarMulti a = map (* a)


public export
transpose : {rows: Nat} -> {cols: Nat} ->  (martix : Martix rows cols n)  -> Martix cols rows n
transpose (MkMartix vects) = MkMartix $ Data.Vect.transpose vects

-- 共轭
-- 共轭转置
-- 行列式
-- 特征值与特征向量


public export
(*) : Num a => {l: Nat} -> {m: Nat} -> {n: Nat} -> (martix1 : Martix m l a) ->  (martix2 : Martix l n a)  -> Martix m n a
(*) (MkMartix martix1) martix2 = let (MkMartix martix2') = transpose martix2 in
  MkMartix $ martix1 <&> \r1 => 
                          martix2' <&> \c1 => 
                                        dotProduct r1 c1

public export
minor : (row: Fin (S n)) -> (col: Fin (S n)) -> (martix1 : Martix (S n) (S n) a)  -> Martix n n a 
minor row col (MkMartix martix1)  = fromVects . deleteAt row . map (deleteAt col) $ martix1

public export
algeCofactor : Neg a => (row: Fin (S n)) -> (col: Fin (S n)) -> (martix1 : Martix (S n) (S n) a)  -> Martix n n a
algeCofactor row col martix1 = minor row col martix1 <&> (* sign)
  where
    sign : a
    sign = if mod ((cast row) + (cast col)) 2 == 0 then 1 else -1

public export
trace : Num a => Martix n n a -> a
trace (MkMartix martix1) = sum $ diag martix1
