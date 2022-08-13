module Test.Martix

import Data.Martix
import Data.Vect 

import Tester
import Tester.Runner

-- 测试行内代码
private
testZeros : List Test
testZeros = [
    test "test zeros" $ assertEq (zeros 2 2) $ fromList [ fromList [0,0],fromList [0,0]]
]

private
testOnes : List Test
testOnes = [
    test "test ones" $ assertEq (ones 2 2) $ fromList [ fromList [1,1],fromList [1,1]]
]

export 
tests : List Test
tests = testZeros ++ testOnes

private
main : IO ()
main = do
    success <- runTests $ tests 
    if success
        then putStrLn "工作正常"
        else putStrLn "出错"