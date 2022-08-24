module Main

import Test.First as F
import Tester.Runner


public export
main : IO ()
main = do
    success <- runTests $ F.tests
    if success
        then putStrLn "全部工作正常"
        else putStrLn "有部分功能异常"