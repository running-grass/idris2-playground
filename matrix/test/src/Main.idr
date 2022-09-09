module Main

import Data.Matrix
import Test.Matrix as M
import Tester.Runner


public export
main : IO ()
main = do
    -- print $ prettyShow $ zeros 2 2
    success <- runTests $ M.tests
    if success
        then putStrLn "All testHeading passed"
        else putStrLn "Not all testHeading passed"