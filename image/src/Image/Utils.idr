module Image.Utils

import Data.Buffer
import Data.List
import Data.Vect
import Data.Matrix
import Image.Type


export
toVect : HasIO io => (len: Nat) -> Buffer -> io (len ** Vect len Bits8)
toVect 0 _ = pure (0 ** [])
toVect (S size) buffer = go size $ pure (0 ** [])
  where
  go :  (s: Nat) -> io (n ** Vect n Bits8) -> io (m ** (Vect m Bits8))
  go 0 dvect = do 
    (n ** vs) <- dvect
    b <- getBits8 buffer 0
    pure (_ ** b :: vs)
  go s@(S p) dvect = do 
    (n ** vs) <- dvect
    b <- getBits8 buffer $ cast s
    go p $ pure (_ ** b :: vs)

export
groupList :  (m: Nat) -> (n: Nat) -> List Bits8 -> Maybe $ Vect m (Vect n RGB)
groupList 0 _ _ = Just []
groupList m 0 _ = Just $ replicate m []
groupList (S m) n (ls) = let (st, sts) = splitAt (n * 3) ls
   in do
    (s ** hea) <- getTup st (0 ** [])
    tai <- groupList m n sts
    Just $ ((believe_me . reverse $ hea) :: (  tai))
   where 
    getTup : List Bits8 -> (n' ** Vect n' RGB) -> Maybe $ (nn ** Vect nn RGB)
    getTup (a1 :: a2 :: a3 :: xs) (vn ** vs) = getTup (xs) (_ ** (fromTuple (a1, a2, a3) :: vs))
    getTup [] dv@(vn ** vs) = if vn == n then Just dv else Nothing
    getTup _ _ = Nothing

