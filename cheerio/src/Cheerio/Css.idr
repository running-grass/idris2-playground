module Cheerio.Css

import Cheerio.Data
import Cheerio.Utils

-- css

%foreign "node:lambda: (primToMaybe, cssName, cheerio) => primToMaybe(cheerio.css(cssName))"
prim_css : (String -> Maybe String) -> String -> Cheerio -> PrimIO (Maybe String)

export
css : HasIO io => String -> Cheerio -> io (Maybe String)
css cssName cheerio = primIO $ prim_css primToMaybe cssName cheerio
