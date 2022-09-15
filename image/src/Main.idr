module Main
import System.File
import System.File.Buffer
import Data.Buffer
import System.FFI
import Data.Buffer

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


%foreign (rlib "getString")
prim_getString : Ptr String -> PrimIO String

getString : HasIO io => Ptr String -> io String
getString ptr = primIO $ prim_getString ptr

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
      -- TODO 向后错了四个字节
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
