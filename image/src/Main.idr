module Main
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

toVect : HasIO io => Buffer -> io (size ** Vect size Bits8)
toVect buffer = let size = !(rawSize buffer)
  in go (cast size) $ pure (0 ** [])
  where
  go : Nat -> io (n ** Vect n Bits8) -> io (m ** Vect m Bits8)
  go 0 dvect = dvect
  go s dvect = do 
    (n ** vs) <- dvect
    b <- getBits8 buffer $ cast s
    pure $ (S n ** b :: vs)

-- toMatrix : {m: Nat} -> {n: Nat} -> Vect (m * n * 3) Bits8 -> Matrix m n (Bits8, Bits8, Bits8)
-- toMatrix vect = ?t

main : IO ()
main = do
  let filePath = "/Users/grass/workspace/idris2-playground/image/res/grass.jpg"
  -- let filePath = "/Users/grass/workspace/idris2-playground/image/res/fengjing.jpeg"
  jpg <- readJpeg filePath

  let width : Int = getField jpg "image_width"
      height : Int = getField jpg "image_height"
      bufferPtr: Ptr String = getField jpg "buffer"
      size = width * height * 3
  buffer' <- newBuffer $ size

  case buffer' of
    Nothing => pure ()
    Just buffer => do 
      setString' buffer bufferPtr size

      putStrLn $ "total size: " ++ show !(rawSize buffer)
      putStrLn "===== head start ======"
      putStrLn $ show !(getBits8 buffer 0)
      putStrLn $ show !(getBits8 buffer 1)
      putStrLn $ show !(getBits8 buffer 2)
      putStrLn $ show !(getBits8 buffer 3)
      putStrLn $ show !(getBits8 buffer 4)
      putStrLn $ show !(getBits8 buffer 5)
      putStrLn $ show !(getBits8 buffer 6)
      putStrLn "===== tail before ======"

      putStrLn $ show !(getBits8 buffer (size - 3))
      putStrLn $ show !(getBits8 buffer (size - 2))
      putStrLn $ show !(getBits8 buffer (size - 1))
      putStrLn "===== tail end ======"
      putStrLn $ show !(getBits8 buffer (size))

      str <- getString buffer 0 (size) 
      let chars = unpack str
          -- bss = fromList cast chars
          c0 : Maybe Bits8 =  (head' chars) <&> cast 
          c' : Maybe Bits8 =  (last' chars) <&> cast 
          -- b = index 0 bss
      putStrLn $ show $  c0 
      putStrLn $ show $  c'
      -- pure () 
