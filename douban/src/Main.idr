module Main

-- import System
-- import Data.String.Parser
-- import Language.XML


%foreign "node:lambda: n => n + n"
prim_getEnv : Int32 -> PrimIO Int32


getEnv : HasIO io => Int32 -> io Int32
getEnv n = primIO $ prim_getEnv n


%foreign "node:lambda: n => require('cheerio').load('<h2 class=\"title\">Hello world</h2>')('h2.title').text()"
prim_cheerio : String -> PrimIO String


cheerio : HasIO io => String -> io String
cheerio n = primIO $ prim_cheerio n


main : IO ()
main = do
  res <- cheerio ""
  putStrLn res