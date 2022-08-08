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
- [x] parent
- [ ] parents
- [ ] childrens


## Attributes
- [x] attr 
- [ ] data
- [ ] prop

## Manipulation
- [x] html
- [x] text