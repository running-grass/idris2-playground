module Image.Jpeg

import System.File
import System.File.Buffer
import Data.Buffer
import Data.String
import System.FFI
import Data.Vect
import Image.Type
import Image.Utils
import Data.Matrix

rlib : String -> String
rlib fn = "C:" ++ fn ++ ",libidrisjpeg"

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

export
loadJpeg : HasIO io => (filePath: String) -> io (Maybe (m ** n ** Image m n))
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
        (Just vs) => pure $ Just $ (height' ** width' ** MkImage $ fromVects vs)
        _ =>  pure Nothing
