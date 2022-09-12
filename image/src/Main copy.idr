module Main
import System.File
import System.File.Buffer
import Data.Buffer


rlib : String -> String
rlib fn = "C:" ++ fn ++ ",libidrisjpeg"


%foreign (rlib "showStr")
prim__putStr : String -> PrimIO ()

export
showStr : HasIO io => String -> io ()
showStr str = primIO $ prim__putStr str



%foreign (rlib "read_JPEG_file")
prim__readJpeg : String -> PrimIO Int

export
readJpeg : HasIO io => String -> io Int
readJpeg str = primIO $ prim__readJpeg str


%foreign (rlib "getString")
export
getString : Ptr String -> String

%foreign (rlib "mkString")
export
mkString : String -> Ptr String

%foreign (rlib "nullString")
export
nullString : Ptr String

%foreign (rlib "isNullString")
prim__isNullString : Ptr String -> Int

export
isNullString : Ptr String -> Bool
isNullString str = not $ prim__isNullString str == 0

%foreign (rlib "readline")
prim__readline : String -> PrimIO (Ptr String)

export
readline : HasIO io => String -> io (Maybe String)
readline s
    = do mstr <- primIO $ prim__readline s
         pure $ if isNullString mstr
                   then Nothing
                   else Just (getString mstr)

%foreign (rlib "add_history")
prim__add_history : String -> PrimIO ()

export
addHistory : HasIO io => String -> io ()
addHistory s = primIO $ prim__add_history s

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




processBuffer : Buffer -> IO ()
processBuffer buffer = do
  type <- getString buffer 0 2
  size <- getInt32 buffer 2
  startPos <- getInt32 buffer 10

  dibSize <- getInt32 buffer 14
  width <- getInt32 buffer 18
  height <- getInt32 buffer 22
  face <- getBits16 buffer 26
  bits <- getBits16 buffer 28
  zipFun <- getInt32 buffer 30
  imgSize <- getInt32 buffer 34
  diph <- getInt32 buffer 38
  dipv <- getInt32 buffer 42
  colors <- getInt32 buffer 46
  colors1 <- getInt32 buffer 50
  
  r <- getBits8 buffer 138
  g <- getBits8 buffer 139
  b <- getBits8 buffer 140
  a <- getBits8 buffer 141
  g2 <- getBits8 buffer 144
  b2 <- getBits8 buffer 149

  putStrLn $ show r
  putStrLn $ show g
  putStrLn $ show b
  putStrLn $ show a
  putStrLn $ show g2
  putStrLn $ show bits
  putStrLn $ show zipFun


main : IO ()
main = do
  let filePath = "/Users/grass/workspace/idris2-playground/image/res/grass.jpg"
  str <- createBufferFromFile filePath
  ignore $ readJpeg filePath
  -- case str of
  --   (Right buffer) => processBuffer buffer
  --   _ => pure ()
