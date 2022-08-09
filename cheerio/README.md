# [WIP] Idris2 cheerio 


# Usage
## add idris package `cheerio` to `.ipkg` file

## install npm package `cheerio`

```bash
npm i cheerio
```

## add code to Main.idr
```idris
module Main

import Cheerio

main : IO ()
main = do
  che <- load "<h2 class=\"title\"><p id=\"world\">Hello world22</p><span>span123</span></h2>"
  h2 <- query "h2" che
  p <- find "p" h2
  t <- attr "id" p
  putStrLn $ show t
```

## run with pack

# Develop

```
pnpm i
make run
```

# Feature

## Traversing
- [x] findd
- [x] parentBy
- [x] parent
- [x] parentsBy
- [x] parents
- [x] parentsUntil
- [x] closestBy
- [x] closest
- [x] nextBy
- [x] next
- [x] nextAllBy
- [x] nextAll
- [x] nextUntil
- [x] prevBy
- [x] prev
- [x] prevAllBy
- [x] prevAll
- [x] prevUntil
- [x] siblingsBy
- [x] siblings
- [x] childrenBy
- [x] children
- [x] contentsBy
- [x] contents
- [ ] each
- [ ] map
- [ ] filterBY
- [ ] filterWith
- [ ] filterArray
- [x] is 
- [ ] not
- [ ] has
- [x] first
- [x] last
- [x] eq
- [ ] getBy
- [ ] get
- [x] toArray
- [ ] index
- [ ] slice
- [x] end


## Attributes
- [x] attr 
- [ ] data
- [ ] prop

## Manipulation
- [x] html
- [x] text