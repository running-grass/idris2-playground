module Cheerio

import Cheerio.Data

%foreign "node:lambda: html => require('cheerio').load(html)"
prim_load : String -> PrimIO Cheerio

load : HasIO io => String -> io Cheerio
load n = primIO $ prim_load n

%foreign "node:lambda: (selector, $) => $(selector)"
prim_find : String -> Cheerio -> PrimIO Cheerio

find : HasIO io => String -> Cheerio -> io Cheerio
find selector cheerio = primIO $ prim_find selector cheerio



%foreign "node:lambda: ($) => $.text()"
prim_text : Cheerio -> PrimIO String

text : HasIO io => Cheerio -> io String
text cheerio = primIO $ prim_text cheerio



%foreign "node:lambda: n => require('cheerio').load('<h2 class=\"title\">Hello world</h2>')('h2.title').text()"
prim_cheerio : String -> PrimIO String

cheerio : HasIO io => String -> io String
cheerio n = primIO $ prim_cheerio n


main : IO ()
main = do
  che <- load "<h2 class=\"title\">Hello world</h2>"
  -- printLn che  
  h2 <- find "h2" che
  t <- text h2
  putStrLn t