module Cheerio.Attributes

import Cheerio.Data
import Cheerio.Utils

%foreign "node:lambda: (primToMaybe, name, cheerio) => primToMaybe(cheerio.attr(name))"
prim_attr : (String -> Maybe String) -> String -> Cheerio -> PrimIO (Maybe String)

export
attr : HasIO io => String -> Cheerio -> io (Maybe String)
attr name cheerio = primIO $ prim_attr primToMaybe name cheerio


%foreign "node:lambda: (primToMaybe, cheerio) => primToMaybe(cheerio.val())"
prim_val : (String -> Maybe String) -> Cheerio -> PrimIO (Maybe String)

export
val : HasIO io => Cheerio -> io (Maybe String)
val cheerio = primIO $ prim_val primToMaybe cheerio

