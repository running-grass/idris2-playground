module Image.Bmp

import System.File
import System.File.Buffer
import Data.Buffer

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
  putStrLn $ show startPos


main : IO ()
main = do
  let filePath = "/Users/grass/workspace/idris2-playground/image/res/red.bmp"
  str <- createBufferFromFile filePath
  -- ignore $ readJpeg filePath
  case str of
    (Right buffer) => processBuffer buffer
    _ => pure ()
