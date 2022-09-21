module Image
import System.File
import System.File.Buffer
import Data.Buffer
import Data.String
import System.FFI
import Data.Vect
import Data.Matrix

rlib : String -> String
rlib fn = "C:" ++ fn ++ ",libidrisjpeg"

export
data RGBBuffer = MkRGBBuffer AnyPtr

public export
RGB : Type
RGB = (Bits8, Bits8, Bits8)

-- public export
-- RGBImage : Nat -> Nat -> Type
-- RGBImage m n = Vect m $ Vect n RGB

JpegDecompress : Type
JpegDecompress = Struct "jpeg_decompress_struct" [("image_width", Int), ("image_height", Int), ("buffer", Ptr String)]


%foreign (rlib "setBufferString2")
prim__setString : Buffer -> (val : Ptr String) -> (offset : Int) -> PrimIO ()

setString' : HasIO io => Buffer -> (val : Ptr String) -> (length : Int) -> io ()
setString' buf  val len
    = primIO (prim__setString buf val len)

%foreign (rlib "read_JPEG_file")
prim__readJpeg : String -> PrimIO JpegDecompress

readJpeg : HasIO io => String -> io JpegDecompress
readJpeg str = primIO $ prim__readJpeg str

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

-- groupVect :  (m: Nat) -> (n: Nat) -> Vect (m * n) a -> Vect m (Vect n a)
-- groupVect 0 _ _ = []
-- groupVect m 0 _ = replicate m []
-- groupVect (S m') n vect = let (st, sts) = splitAt n vect
--    in (st :: groupVect m' n sts)
 

groupList :  (m: Nat) -> (n: Nat) -> List a -> Maybe $ Vect m (Vect n (a, a, a))
groupList 0 _ _ = Just []
groupList m 0 _ = Just $ replicate m []
groupList (S m) n (ls) = let (st, sts) = splitAt (n * 3) ls
   in do
    (s ** hea) <- getTup st (0 ** [])
    tai <- groupList m n sts
    Just $ ((believe_me . reverse $ hea) :: (  tai))
   where 
    getTup : List a -> (n' ** Vect n' (a,a,a)) -> Maybe $ (nn ** Vect nn (a,a,a))
    getTup (a1 :: a2 :: a3 :: xs) (vn ** vs) = getTup (xs) (_ ** ((a1, a2, a3) :: vs))
    getTup [] dv@(vn ** vs) = if vn == n then Just dv else Nothing
    getTup _ _ = Nothing

export
loadJpeg : HasIO io => (filePath: String) -> io (Maybe (m ** n ** Matrix m n RGB))
loadJpeg filePath = do
  jpg <- readJpeg filePath

  let width : Int = getField jpg "image_width"
      height : Int = getField jpg "image_height"
      width' : Nat = cast width
      height' : Nat = cast height
      bufferPtr: Ptr String = getField jpg "buffer"
      size' : Nat = width' * height'
      size : Nat = mult size' 3
      sizeI : Int = cast size
  buffer' <- newBuffer $ cast size

  case buffer' of
    Nothing => pure Nothing
    Just buffer => do 
      setString' buffer bufferPtr $ cast size

      (n ** (chars)) <- toVect size buffer
      chars' <- pure $ toList chars

      case groupList (height') (width') chars' of
        (Just vs) => pure $ Just $ (height' ** width' ** fromVects vs)
        _ =>  pure Nothing


main : IO ()
main = do
  let filePath = "./test/grass32.jpg"
  (Just (_ ** _ ** jpg)) <- loadJpeg filePath
    | Nothing => pure ()
  putStrLn $ show jpg
  pure ()