module Cheerio.Utils

-- import Data.List

%foreign "node:lambda: (_, nothing, just, x) => { if (x === undefined) { return nothing } else { return just(x);}} "
prim_toMaybe : Maybe a -> (a -> Maybe a) -> a -> Maybe a

export
primToMaybe : a -> Maybe a
primToMaybe x = prim_toMaybe Nothing Just x

%foreign "node:lambda: (_, nil, cons, arr) => !Array.isArray(arr) ? nil : arr.reduce((acc,x) => cons(x)(acc), nil)"
prim_arrayToList : List a -> (a -> List a -> List a) -> AnyPtr -> Ptr (List a)

public export
arrayToList : AnyPtr -> Ptr (List a)
arrayToList x = prim_arrayToList Nil (::) x