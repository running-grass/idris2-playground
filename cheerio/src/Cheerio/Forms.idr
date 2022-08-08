module Cheerio.Forms

import Cheerio.Data

-- serialize

%foreign "node:lambda: cheerio => cheerio.serialize()"
prim_serialize : Cheerio -> PrimIO String

export
serialize : HasIO io => Cheerio -> io String
serialize cheerio = primIO $ prim_serialize cheerio