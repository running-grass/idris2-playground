module Cheerio

import public Cheerio.Data

import public Cheerio.Css
import public Cheerio.Forms
import public Cheerio.Traversing
import public Cheerio.Attributes
import public Cheerio.Manipulation

import Data.Maybe

%foreign "node:lambda: html => require('cheerio').load(html)"
prim_load : String -> PrimIO CheerioApi

export
load : HasIO io => String -> io CheerioApi
load n = primIO $ prim_load n


%foreign "node:lambda: (selector, $) => $(selector)"
prim_query : String -> CheerioApi -> PrimIO Cheerio

export
query : HasIO io => String -> CheerioApi -> io Cheerio
query selector cheerioapi = primIO $ prim_query selector cheerioapi



main : IO ()
main = do
  che <- load "<h2 class=\"title\"><p id=\"world\">Hello world22</p><span>span123</span></h2>"
  h2 <- query "h2" che
  p <- find "p" h2
  t <- attr "id" p
  putStrLn $ fromMaybe "nothing" t