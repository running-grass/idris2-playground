module Cheerio.Traversing

import Cheerio.Data

-- find

%foreign "node:lambda: (selector, cheerio) => cheerio.find(selector)"
prim_find : String -> Cheerio -> PrimIO Cheerio

export
find : HasIO io => String -> Cheerio -> io Cheerio
find selector cheerio = primIO $ prim_find selector cheerio

-- parent

%foreign "node:lambda: cheerio => cheerio.parent()"
prim_parent :  Cheerio -> PrimIO Cheerio

export
parent : HasIO io => Cheerio -> io Cheerio
parent cheerio = primIO $ prim_parent cheerio


-- parent by selector

%foreign "node:lambda: (selector, cheerio) => cheerio.parent(selector)"
prim_parentBy : String -> Cheerio -> PrimIO Cheerio

export
parentBy : HasIO io => String -> Cheerio -> io Cheerio
parentBy selector cheerio = primIO $ prim_parentBy selector cheerio

