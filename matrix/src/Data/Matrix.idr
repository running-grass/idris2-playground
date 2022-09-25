module Data.Matrix

import Data.Vect
import Data.String
import Data.Zippable
import Data.Maybe

export
data Matrix : Nat -> Nat -> Type -> Type where
  MkMatrix : Vect rows (Vect cols a) -> Matrix rows cols a

public export
Eq a => Eq (Matrix rows cols a) where
  (MkMatrix lv) == (MkMatrix rv) = lv == rv

public export
Functor (Matrix rows cols) where
  map f (MkMatrix ma) = MkMatrix $ map (map f) ma

public export
Zippable (Matrix rows cols) where
  zipWith f (MkMatrix lv) (MkMatrix rv)  = MkMatrix $ zipWith (zipWith f) lv rv
  unzipWith f v  = let ma = (map f v) in (fst <$> ma,snd <$> ma)

  zipWith3 f (MkMatrix a) (MkMatrix b)  (MkMatrix c) = MkMatrix $ zipWith3 (zipWith3 f) a b c
  unzipWith3 f v  = let ma = (map f v) in (fst <$> ma, fst . snd <$> ma, snd . snd <$> ma)


public export
fromVects : Vect rows (Vect cols a) -> Matrix rows cols a
fromVects = MkMatrix


public export
toVects : Matrix rows cols a -> Vect rows (Vect cols a) 
toVects (MkMatrix v) = v

prettyShow : Show n => (matrix : Matrix rows cols n) -> String
prettyShow (MkMatrix matrix) = (joinBy "\n" $ map show $ toList matrix) ++ "\n"

public export
Show a => Show (Matrix rows cols a) where
  show = prettyShow

private 
dotProduct : Num a => Vect n a -> Vect n a -> a
dotProduct x y = sum $ zipWith (*) x y


public export
fill : (rows : Nat) -> (cols : Nat) -> t -> (Matrix rows cols t)
fill rows cols t0 = MkMatrix $ replicate rows $ replicate cols t0

public export
zeros : (rows : Nat) -> (cols : Nat) -> (Matrix rows cols Int)
zeros rows cols = fill rows cols 0

public export
ones : (rows : Nat) -> (cols : Nat) -> (Matrix rows cols Int)
ones rows cols = fill rows cols  1

public export
updateAt : (row : Fin rows) -> (col : Fin cols) -> (updateFun: a -> a) -> (Matrix rows cols a) -> Matrix rows cols a
updateAt row col f (MkMatrix vects) = MkMatrix $ updateAt row (updateAt col f) vects

public export
replaceAt : (row : Fin rows) -> (col : Fin cols) -> (ele) -> (Matrix rows cols ele) -> (Matrix rows cols ele)
replaceAt row col ele = updateAt row col $ const ele

public export
findIndex : (ele -> Bool) ->  (Matrix rows cols ele) -> Maybe (Fin rows, Fin cols)
findIndex p (MkMatrix vects) = go vects
  where
    go : Vect a (Vect cols ele)  -> Maybe (Fin a, Fin cols)
    go [] = Nothing
    go (v :: vs) = case findIndex p v of
      Just fc => Just (FZ, fc)
      Nothing => case go vs of
                  Just (fa, fb) => Just (FS fa, fb)
                  Nothing => Nothing

-- public export
mapVectWithIndex : {auto len: Nat} -> (ele -> Fin len -> b) -> Vect len ele -> Vect len b
mapVectWithIndex f as = zip range as <&> \(idx, e) => f e idx

public export
mapWithIndex : {auto rows: Nat} -> {auto cols: Nat} -> (ele -> (Fin rows, Fin cols) -> b) ->  Matrix rows cols ele ->  Matrix rows cols b
mapWithIndex f (MkMatrix vects) = MkMatrix $ zip range vects <&> \(row, v) => zip range v <&> \(col, e) => f e (row, col)


filter' : (ele -> Bool) ->  (Matrix rows cols ele) -> List ele
filter' p (MkMatrix vects) = filter p lists
  where
    lists : List ele
    lists = concat . map toList $ vects

public export
findIndices : {auto rows: Nat} -> {auto cols: Nat} ->  (ele -> Bool) ->  (Matrix rows cols ele) -> List (Fin rows, Fin cols)
findIndices p matrix =  mapMaybe id $ filter' isJust mapped
  where
    mapped : Matrix rows cols (Maybe (Fin rows, Fin cols))
    mapped = mapWithIndex {rows = rows} {cols = cols} (\el,(fr, fc) => if p el then (Just (fr, fc)) else Nothing) matrix

-- a : Matrix rows cols String -> Matrix rows cols String
public export
eye : (rows : Nat) -> (cols : Nat) -> Matrix rows cols Int
eye rows cols  = go (rangeFromTo 0 $ min rows cols) (zeros rows cols)
  where
    go : List Nat -> Matrix rows cols Int -> Matrix rows cols Int
    go [] matrix = matrix
    go (x :: xs) matrix = case (natToFin x rows, natToFin x cols) of
                        (Just fx, Just fy) => replaceAt fx fy 1 $ go xs matrix 
                        (_, _) => go xs matrix

public export
identity : (rows : Nat)  -> Matrix rows rows Int
identity rows = eye rows rows


public export
repmat : Num ele => Vect n ele -> (rows : Nat) -> (cols : Nat) -> Matrix rows (cols * n) ele
repmat vect rows cols = MkMatrix $ replicate rows $ concat $  Data.Vect.replicate cols vect

public export
(+) : Num n => (matrix1 : Matrix rows cols n) ->  (matrix2 : Matrix rows cols n)  -> Matrix rows cols n
(+) = zipWith (+)

public export
(-) : Neg n => (matrix1 : Matrix rows cols n) ->  (matrix2 : Matrix rows cols n)  -> Matrix rows cols n
(-) = zipWith (-)

public export
scalarMulti : Num a => a -> (matrix1 : Matrix rows cols a) -> Matrix rows cols a
scalarMulti a = map (* a)


public export
transpose : {rows: Nat} -> {cols: Nat} ->  (matrix : Matrix rows cols n)  -> Matrix cols rows n
transpose (MkMatrix vects) = MkMatrix $ Data.Vect.transpose vects

-- 共轭
-- 共轭转置
-- 行列式
-- 特征值与特征向量


public export
(*) : Num a => {l: Nat} -> {m: Nat} -> {n: Nat} -> (matrix1 : Matrix m l a) ->  (matrix2 : Matrix l n a)  -> Matrix m n a
(*) (MkMatrix matrix1) matrix2 = let (MkMatrix matrix2') = transpose matrix2 in
  MkMatrix $ matrix1 <&> \r1 => 
                          matrix2' <&> \c1 => 
                                        dotProduct r1 c1

public export
minor : (row: Fin (S n)) -> (col: Fin (S n)) -> (matrix1 : Matrix (S n) (S n) a)  -> Matrix n n a 
minor row col (MkMatrix matrix1)  = fromVects . deleteAt row . map (deleteAt col) $ matrix1

public export
algeCofactor : Neg a => (row: Fin (S n)) -> (col: Fin (S n)) -> (matrix1 : Matrix (S n) (S n) a)  -> Matrix n n a
algeCofactor row col matrix1 = minor row col matrix1 <&> (* sign)
  where
    sign : a
    sign = if mod ((cast row) + (cast col)) 2 == 0 then 1 else -1

public export
trace : Num a => Matrix n n a -> a
trace (MkMatrix matrix1) = sum $ diag matrix1


export
flipVer : Matrix m n a -> Matrix m n a
flipVer (MkMatrix vects) = MkMatrix $ reverse vects

export
flipHor : Matrix m n a -> Matrix m n a
flipHor (MkMatrix vects) = MkMatrix $ map reverse vects