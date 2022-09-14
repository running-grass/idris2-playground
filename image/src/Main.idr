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


-- %foreign (rlib "getRGBList")
-- prim_get_rgb : (List Bits8) -> (Bits8 -> List Bits8 -> List Bits8) -> RGBBuffer -> PrimIO (List Bits8)


-- getRGBBuffer : HasIO io => RGBBuffer -> io (List Bits8)
-- getRGBBuffer buf = primIO $ prim_get_rgb Nil (::) buf

%foreign (rlib "showStr")
prim__putStr : String -> PrimIO ()

export
showStr : HasIO io => String -> io ()
showStr str = primIO $ prim__putStr str



%foreign (rlib "read_JPEG_file")
prim__readJpeg : String -> PrimIO JpegDecompress

export
readJpeg : HasIO io => String -> io JpegDecompress
readJpeg str = primIO $ prim__readJpeg str


%foreign (rlib "getString")
prim_getString : Ptr String -> PrimIO String

getString : HasIO io => Ptr String -> io String
getString ptr = primIO $ prim_getString ptr

%foreign (rlib "getBuffer")
prim_getBuffer : Buffer -> AnyPtr -> PrimIO ()

getBuffer : HasIO io => Buffer -> AnyPtr -> io ()
getBuffer  buf ptr = primIO $ prim_getBuffer buf ptr

%foreign (rlib "mkString")
mkString : String -> Ptr String

%foreign (rlib "nullString")
nullString : Ptr String

%foreign (rlib "isNullString")
prim__isNullString : Ptr String -> Int

export
isNullString : Ptr String -> Bool
isNullString str = not $ prim__isNullString str == 0

-- %foreign (rlib "readline")
-- prim__readline : String -> PrimIO (Ptr String)

-- export
-- readline : HasIO io => String -> io (Maybe String)
-- readline s
--     = do mstr <- primIO $ prim__readline s
--          pure $ if isNullString mstr
--                    then Nothing
--                    else Just (getString mstr)

-- %foreign (rlib "add_history")
-- prim__add_history : String -> PrimIO ()

-- export
-- addHistory : HasIO io => String -> io ()
-- addHistory s = primIO $ prim__add_history s

%foreign (rlib "idrisrl_setCompletion")
prim__setCompletion : (String -> Int -> PrimIO (Ptr String)) -> PrimIO ()

export
setCompletionFn : HasIO io => (String -> Int -> IO (Maybe String)) -> io ()
setCompletionFn fn
    = primIO $ prim__setCompletion $ \s, i => toPrim $
          do mstr <- fn s i
             case mstr of
                  Nothing => pure nullString
                  Just str => pure (mkString str)

main : IO ()
main = do
  let filePath = "/Users/grass/workspace/idris2-playground/image/res/grass.jpg"
  jpg <- readJpeg filePath
  putStrLn $ "heelo"

  let width : Int = getField jpg "image_width"
      height : Int = getField jpg "image_height"
      bufferPtr: Ptr String = getField jpg "buffer"
  putStrLn $ show width
  putStrLn $ show height
  
  buffer' <- newBuffer $ width * height * 3

  case buffer' of
    Nothing => pure ()
    Just buffer => do 
      setString buffer 0 !(getString bufferPtr)
      s <- getBits8 buffer 400
      putStrLn $ show s
      putStrLn $ show $ !(rawSize buffer)

