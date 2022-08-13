module Main

import Cheerio
import Data.String

readHello : IO ()
readHello = do
  name <- getLine
  putStrLn $ "Hello " ++ name ++ "!"

friendlyReadHello : IO ()
friendlyReadHello = do
  putStrLn "Please enter your name."
  readHello


%foreign "node:lambda: cmd => require('child_process').execSync(cmd)"
prim_run : String -> PrimIO String

export
nodeRun : HasIO io => String -> io String
nodeRun cmd = primIO $ prim_run cmd




main : IO ()
main = do
  str <- nodeRun "curl https://movie.douban.com/top250"
  -- putStrLn str
  che <- load str
  h2 <- query ".hd .title:first-child" che
  -- h2 <- query "h1" che
  h2l <- toArray h2
  t <- traverse (flip wrap che) h2l
  t1 <- traverse text t
  -- t2 <- ?tt t
  putStrLn $ joinBy "," t1
