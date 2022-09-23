module Image
import System.File
import System.File.Buffer
import Data.Buffer
import Data.String
import System.FFI
import Data.Vect
import Data.Matrix

import public Image.Type
import public Image.Jpeg

main : IO ()
main = do
  let filePath = "./test/grass32.jpg"
  (Just (_ ** _ ** jpg)) <- loadJpeg filePath
    | Nothing => pure ()
  putStrLn $ show jpg
  pure ()