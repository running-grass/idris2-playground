module Main

import Image

import Tester.Runner
import Data.Matrix
import Data.Vect 
import Tester
import System.Directory

jpgMatrix : Matrix 2 3 RGB
jpgMatrix = fromVects [
    [fromTuple (217, (233, 171)),fromTuple (229, (245, 183)),fromTuple (193, (209, 147))],
    [fromTuple (214, (230, 168)),fromTuple (235, (251, 189)),fromTuple (210, (226, 164))]
    ]

private
testsJpg : Matrix 2 3 RGB -> List Test
testsJpg vects = [
    test "test load jpg" $ assertEq vects jpgMatrix
]

private
testsFunc : List Test
testsFunc = [
    test "test flip hor" $ assertEq jpgMatrix $ flipHor . flipHor $ jpgMatrix
    , test "test flip ver" $ assertEq jpgMatrix $ flipVer . flipVer $ jpgMatrix
    , test "test flip ver and hor" $ assertEq jpgMatrix $ flipHor . flipVer . flipVer . flipHor$ jpgMatrix
]


public export
main : IO ()
main = do
    (Just dir) <- currentDir
        | Nothing => putStrLn "get current directory fail"
    let filePath = dir ++ "/test/grass32.jpg"
    (Just (2 ** 3 ** (MkImage jpg))) <- loadJpeg filePath
        | Just _ => putStrLn "image size error"
        | Nothing => putStrLn "loadJpeg error"
    success <- runTests $ testsJpg jpg ++ testsFunc
    if success
        then putStrLn "All testHeading passed"
        else putStrLn "Not all testHeading passed"