module Main

import Test.Martix as M
import Tester.Runner


public export
main : IO ()
main = do
    success <- runTests $ M.tests
    if success
        then putStrLn "All testHeading passed"
        else putStrLn "Not all testHeading passed"