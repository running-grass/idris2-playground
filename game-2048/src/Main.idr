module Main

import System
import System.Random

import Data.Martix
import Data.String
import Data.Fin
import Data.Nat
import Data.List
import Control.App
import Control.App.Console
import Data.Vect

Tile : Type
Tile = Int

record  GameField (rows: Nat) (cols: Nat) where
  constructor MkGameField
  gameField : Martix rows cols Tile


Show (GameField r c) where
  show (MkGameField s) = show s

Eq (GameField r c) where
  (MkGameField s1) == (MkGameField s2) = s1 == s2

-- Functor (GameField r c) where
--   map f (MkGameField s1) = MkGameField $ f s1

toMartix :  GameField rows cols -> Martix rows cols Int
toMartix (MkGameField m) = m

up : Vect (len) Int -> Vect (len) Int
up [] = []
up (0 :: as) = snoc (up as) 0
up (a :: as) = a :: up as

join : Vect len Int -> Vect len Int
join (x :: y :: as) = if x == y then snoc ([x + y] ++ join as) 0  else (x :: join (y :: as))
join (x :: as) = x :: join as
join [] = []

moveLeft :  GameField rows cols -> GameField rows cols
moveLeft (MkGameField m) = MkGameField $ fromVects $ map (join . up) $ toVects m

moveRight : GameField rows cols-> GameField rows cols
moveRight (MkGameField m) = MkGameField $ fromVects $ (map (reverse . join . up . reverse)) $ toVects m

moveUp : {rows : Nat} -> {cols : Nat} -> GameField rows cols-> GameField rows cols
moveUp  (MkGameField m) = MkGameField $ transpose $ toMartix $ moveLeft $  MkGameField $ transpose  m


moveDown : {rows : Nat} -> {cols : Nat} -> GameField rows cols-> GameField rows cols
moveDown  (MkGameField m) = MkGameField $ transpose $ toMartix $ moveRight $  MkGameField $ transpose  m

-- 获取可以使用的titles
getFreeTiles : {rows: Nat} -> {cols: Nat} -> GameField rows cols -> List (Fin rows, Fin cols)
getFreeTiles (MkGameField martix) = findIndices {rows} {cols} (== 0) martix

addTileToRandomPos : {rows: Nat} -> {cols: Nat} -> GameField rows cols -> IO (GameField rows cols)
addTileToRandomPos game@(MkGameField martix) = do
  let freeTiles = getFreeTiles game 
  it <- rndFin $ length freeTiles
  case getAt (finToNat it) freeTiles of
    Nothing => pure game
    Just (r,c ) => pure $ MkGameField $ replaceAt r c 2 martix


hasAvailableMoves : GameField rows cols -> Bool
hasAvailableMoves game = True

gameLoop :  {rows: Nat} -> {cols: Nat} -> GameField rows cols ->  App Init ()
gameLoop gameField = do
    gameNext <- primIO $ addTileToRandomPos gameField
    performInput gameNext
    where
      mutual
        performInput : GameField rows cols -> App Init ()
        performInput gameField = do
          print gameField
          if hasAvailableMoves gameField
            then putStrLn "Your command ([l]eft, [r]ight, [u]p, [d]own, [s]top)?"
            else putStrLn "Game over. Press [s] to exit."
          putStrLn "============================================="
          input <- getLine
          case input of
            "l" => newMove moveLeft gameField
            "r" => newMove moveRight gameField
            "u" => newMove moveUp gameField
            "d" => newMove moveDown gameField
            "s" => putStrLn "stop!"
            _   => do 
              putStrLn "错误的命令"
              performInput gameField

        newMove : (GameField rows cols -> GameField rows cols) -> GameField rows cols -> App Init ()
        newMove moveFn gameField = do
          let gameField' = moveFn gameField
          if gameField /= gameField' then gameLoop gameField'
                                    else performInput gameField
        
              
app : App Init ()
app = do
  -- putStrLn "输入行列数以开始游戏："
  -- rows' <- getLine
  -- row <- pure $ stringToNatOrZ rows'
  let row = 5
  game <- pure $ MkGameField $ fill row row 0
  gameLoop game

main : IO ()
main = do 
  run app
  -- putStrLn "输入行列数以开始游戏："
  -- rows' <- getLine
  -- -- row <- pure $ stringToNatOrZ rows'
  -- let row = 5
  -- game <- pure $ MkGameField $ fill row row 0
  -- -- game2 <- addTileToRandomPos game

  -- gameLoop game