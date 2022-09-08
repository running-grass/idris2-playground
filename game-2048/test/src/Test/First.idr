module Test.First

import Tester
import Tester.Runner

-- 测试行内代码
private
testOne : List Test
testOne = [
    test "test one" $ assertEq 1 1
]

private
testTwo : List Test
testTwo = [
    test "test two" $ assertEq 2 2
]
export 
tests : List Test
tests = testOne ++ testTwo

private
main : IO ()
main = do
    success <- runTests $ tests 
    if success
        then putStrLn "正常"
        else putStrLn "出错"