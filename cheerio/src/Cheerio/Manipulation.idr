module Cheerio.Manipulation

import Cheerio.Data
import Cheerio.Utils

-- text

%foreign "node:lambda: cheerio => cheerio.text()"
prim_text : Cheerio -> PrimIO String

export
text : HasIO io => Cheerio -> io String
text cheerio = primIO $ prim_text cheerio

-- html

%foreign "node:lambda: (primToMaybe, cheerio) => primToMaybe(cheerio.text())"
prim_html : (String -> Maybe String) -> Cheerio -> PrimIO (Maybe String)

export
html : HasIO io => Cheerio -> io (Maybe String)
html cheerio = primIO $ prim_html primToMaybe cheerio
