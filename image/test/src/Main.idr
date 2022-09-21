module Main

import Image
import Tester.Runner
import Data.Matrix
import Data.Vect 
import Tester
import System.Directory

jpgMatrix : Matrix 2 3 RGB
jpgMatrix = fromVects [[(217, (233, 171)), (229, (245, 183)), (193, (209, 147))],[(214, (230, 168)), (235, (251, 189)), (210, (226, 164))]]

private
tests : Matrix 2 3 RGB -> List Test
tests vects = [
    test "test zeros 1" $ assertEq vects jpgMatrix
]

public export
main : IO ()
main = do
    -- (Just dir) <- currentDir
    --     | Nothing => putStrLn "get current directory fail"
    -- putStrLn dir
    let filePath = "/Users/grass/workspace/idris2-playground/image/test/grass32.jpg"
    (Just (2 ** 3 ** jpg)) <- loadJpeg filePath
        | Just _ => putStrLn "image size error"
        | Nothing => putStrLn "loadJpeg error"
    success <- runTests $ tests jpg
    if success
        then putStrLn "All testHeading passed"
        else putStrLn "Not all testHeading passed"