module Cheerio.Utils

%foreign "node:lambda: (a) => a === undefined || a === null"
isNull : (Ptr a) -> Bool


%foreign "node:lambda: (_, nothing, just, x) => { if (x === undefined) { return nothing } else { return just(x);}} "
prim_toMaybe : Maybe a -> (a -> Maybe a) -> a -> Maybe a

export
primToMaybe : a -> Maybe a
primToMaybe x = prim_toMaybe Nothing Just x

