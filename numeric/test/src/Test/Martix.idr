module Test.Martix

import Data.Martix
import Data.Vect 

import Tester
import Tester.Runner

-- 测试行内代码
private
testZeros : List Test
testZeros = [
    test "test zeros 1" $ assertEq (zeros 2 2) $ fromVects [ [0,0], [0,0]]
    , test "test zeros 2" $ assertEq (zeros 1 1) $ fromVects [[0]]
    , test "test zeros 3" $ assertEq (zeros 1 3) $ fromVects [[0,0,0]]
    , test "test zeros 4" $ assertEq (zeros 0 0) $ fromVects []
]

test2D : Martix 2 2 Int
test2D = fromVects [ [1,2], [3,4] ]

private
testOnes : List Test
testOnes = [
    test "test ones 1" $ assertEq (ones 2 2) $ fromVects [ [1,1], [1,1]]
    , test "test ones 2" $ assertEq (ones 1 1) $ fromVects [[1]]
    , test "test ones 3" $ assertEq (ones 1 3) $ fromVects [[1,1,1]]
    , test "test ones 4" $ assertEq (ones 0 0) $ fromVects []
]

testUpdate : List Test
testUpdate = [
    test "test update 1" $ assertEq (updateAt 0 0 (+ 3) $ ones 2 2) $ fromVects [ [4,1], [1,1]]
    , test "test update 2" $ assertEq (updateAt 0 0 (+ 10) $ ones 1 1) $ fromVects [[11]]
    , test "test replace 1" $ assertEq (replaceAt 0 0 10 $ ones 1 1) $ fromVects [[10]]
    , test "test eye 1" $ assertEq (eye 2 2) $ fromVects [[1,0],[0, 1]]
    , test "test eye 2" $ assertEq (eye 3 2) $ fromVects [[1,0],[0, 1],[0,0]]
    , test "test eye 3" $ assertEq (eye 1 3) $ fromVects [[1,0,0]]
    , test "test identity 1" $ assertEq (identity 2) $ fromVects [[1,0],[0, 1]]
    , test "test identity 2" $ assertEq (identity 0) $ fromVects []
    , test "test repmat 1" $ assertEq (repmat [1,2] 0 0) $ fromVects []
    , test "test repmat 2" $ assertEq (repmat [1,2] 2 2) $ fromVects [[1,2,1,2],[1,2,1,2]]
    , test "test repmat 3" $ assertEq (repmat [1,2] 2 0) $ fromVects [[],[]]

    , test "test transpose 1" $ assertEq (Data.Martix.transpose (fromVects [[1,2,3]])) $ fromVects [[1],[2], [3]]
    , test "test transpose 2" $ assertEq (Data.Martix.transpose (fromVects [[1,2,3], [4,5,6]])) $ fromVects [[1,4],[2,5], [3,6]]

    , test "test *" $ assertEq (test2D * (identity 2)) test2D
    , test "test *" $ assertEq (zeros 2 0 * (zeros 0 2)) $ zeros 2 2
    , test "test +" $ assertEq (test2D + (zeros 2 2)) test2D
]

export 
tests : List Test
tests = testZeros ++ testOnes ++ testUpdate

private
main : IO ()
main = do
    success <- runTests $ tests 
    if success
        then putStrLn "工作正常"
        else putStrLn "出错"