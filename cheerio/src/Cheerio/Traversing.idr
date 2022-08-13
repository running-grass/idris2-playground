module Cheerio.Traversing

import Cheerio.Data
import Cheerio.Utils

-- find

%foreign "node:lambda: (selector, cheerio) => cheerio.find(selector)"
prim_find : String -> Cheerio -> PrimIO Cheerio

export
find : HasIO io => String -> Cheerio -> io Cheerio
find selector cheerio = primIO $ prim_find selector cheerio


-- parents by selector
%foreign "node:lambda: (selector, cheerio) => cheerio.parent(selector)"
prim_parensBy : String -> Cheerio -> PrimIO Cheerio

export
parentBy : HasIO io => String -> Cheerio -> io Cheerio
parentBy selector cheerio = primIO $ prim_parensBy selector cheerio


export
parent : HasIO io => Cheerio -> io Cheerio
parent = parentBy ""

-- parents
%foreign "node:lambda: cheerio => cheerio.parents()"
prim_parents :  Cheerio -> PrimIO Cheerio

-- parents by selector
%foreign "node:lambda: (selector, cheerio) => cheerio.parents(selector)"
prim_parenstBy : String -> Cheerio -> PrimIO Cheerio

export
parentsBy : HasIO io => String -> Cheerio -> io Cheerio
parentsBy selector cheerio = primIO $ prim_parenstBy selector cheerio

export
parents : HasIO io => Cheerio -> io Cheerio
parents = parentsBy ""


-- up to but not including the element matched by the selector,
%foreign "node:lambda: (selector, cheerio) => cheerio.parentsUntil(selector)"
prim_parentsUntil : String -> Cheerio -> PrimIO Cheerio

export
parentsUntil : HasIO io => String -> Cheerio -> io Cheerio
parentsUntil selector cheerio = primIO $ prim_parentsUntil selector cheerio


-- closest by selector
%foreign "node:lambda: (selector, cheerio) => cheerio.closest(selector)"
prim_closestBy : String -> Cheerio -> PrimIO Cheerio

export
closestBy : HasIO io => String -> Cheerio -> io Cheerio
closestBy selector cheerio = primIO $ prim_closestBy selector cheerio

export
closest : HasIO io => Cheerio -> io Cheerio
closest = closestBy ""


-- nextBy by selector
%foreign "node:lambda: (selector, cheerio) => cheerio.next(selector)"
prim_nextBy : String -> Cheerio -> PrimIO Cheerio

export
nextBy : HasIO io => String -> Cheerio -> io Cheerio
nextBy selector cheerio = primIO $ prim_nextBy selector cheerio

export
next : HasIO io => Cheerio -> io Cheerio
next = nextBy ""


-- nextAll by selector
%foreign "node:lambda: (selector, cheerio) => cheerio.nextAll(selector)"
prim_nextAll : String -> Cheerio -> PrimIO Cheerio

export
nextAllBy : HasIO io => String -> Cheerio -> io Cheerio
nextAllBy selector cheerio = primIO $ prim_nextAll selector cheerio

export
nextAll : HasIO io => Cheerio -> io Cheerio
nextAll = nextBy ""



-- up to but not including the element matched by the selector,
%foreign "node:lambda: (selector, cheerio) => cheerio.nextUntil(selector)"
prim_nextUntil : String -> Cheerio -> PrimIO Cheerio

export
nextUntil : HasIO io => String -> Cheerio -> io Cheerio
nextUntil selector cheerio = primIO $ prim_nextUntil selector cheerio



-- prevBy by selector
%foreign "node:lambda: (selector, cheerio) => cheerio.prev(selector)"
prim_prevBy : String -> Cheerio -> PrimIO Cheerio

export
prevBy : HasIO io => String -> Cheerio -> io Cheerio
prevBy selector cheerio = primIO $ prim_prevBy selector cheerio

export
prev : HasIO io => Cheerio -> io Cheerio
prev = prevBy ""


-- prevAll by selector
%foreign "node:lambda: (selector, cheerio) => cheerio.prevAll(selector)"
prim_prevAll : String -> Cheerio -> PrimIO Cheerio

export
prevAllBy : HasIO io => String -> Cheerio -> io Cheerio
prevAllBy selector cheerio = primIO $ prim_prevAll selector cheerio

export
prevAll : HasIO io => Cheerio -> io Cheerio
prevAll = prevBy ""


-- up to but not including the element matched by the selector,
%foreign "node:lambda: (selector, cheerio) => cheerio.prevUntil(selector)"
prim_prevUntil : String -> Cheerio -> PrimIO Cheerio

export
prevUntil : HasIO io => String -> Cheerio -> io Cheerio
prevUntil selector cheerio = primIO $ prim_prevUntil selector cheerio



-- siblings by selector
%foreign "node:lambda: (selector, cheerio) => cheerio.siblings(selector)"
prim_siblingsBy : String -> Cheerio -> PrimIO Cheerio

export
siblingsBy : HasIO io => String -> Cheerio -> io Cheerio
siblingsBy selector cheerio = primIO $ prim_siblingsBy selector cheerio

export
siblings : HasIO io => Cheerio -> io Cheerio
siblings = siblingsBy ""



-- children by selector
%foreign "node:lambda: (selector, cheerio) => cheerio.children(selector)"
prim_childrenBy : String -> Cheerio -> PrimIO Cheerio

export
childrenBy : HasIO io => String -> Cheerio -> io Cheerio
childrenBy selector cheerio = primIO $ prim_childrenBy selector cheerio

export
children : HasIO io => Cheerio -> io Cheerio
children = childrenBy ""

-- Gets the children of each element in the set of matched elements, including text and comment nodes.
%foreign "node:lambda: (cheerio) => cheerio.contents()"
prim_contents : Cheerio -> PrimIO Cheerio

export
contents : HasIO io =>Cheerio -> io Cheerio
contents cheerio = primIO $ prim_contents cheerio

-- first
%foreign "node:lambda: (cheerio) => cheerio.first()"
prim_first : Cheerio -> PrimIO Cheerio

export
first : HasIO io =>Cheerio -> io Cheerio
first cheerio = primIO $ prim_first cheerio


-- last
%foreign "node:lambda: (cheerio) => cheerio.last()"
prim_last : Cheerio -> PrimIO Cheerio

export
last : HasIO io =>Cheerio -> io Cheerio
last cheerio = primIO $ prim_last cheerio


-- end
%foreign "node:lambda: (cheerio) => cheerio.end()"
prim_end : Cheerio -> PrimIO Cheerio

export
end : HasIO io =>Cheerio -> io Cheerio
end cheerio = primIO $ prim_end cheerio


-- is
%foreign "node:lambda: (selector, cheerio) => cheerio.is(selector)"
prim_is : String -> Cheerio -> PrimIO Bool

export
is : HasIO io =>  String -> Cheerio -> io Bool
is selector cheerio = primIO $ prim_is selector cheerio

-- is
%foreign "node:lambda: (idx, cheerio) => cheerio.is(idx)"
prim_eq : Int32 -> Cheerio -> PrimIO Cheerio

export
eq : HasIO io =>  Int -> Cheerio -> io Cheerio
eq idx cheerio = primIO $ prim_eq (cast idx) cheerio


-- end
%foreign "node:lambda: (toList, cheerio) => toList(cheerio.toArray())"
prim_toArray : (AnyPtr -> Ptr (List AnyNode)) -> Cheerio -> PrimIO (List AnyNode)

export
toArray : HasIO io =>Cheerio -> io (List AnyNode) 
toArray cheerio = primIO $ prim_toArray arrayToList cheerio