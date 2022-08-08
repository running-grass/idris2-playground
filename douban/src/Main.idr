module Main

import Cheerio

main : IO ()
main = do
  che <- load "<h2 class=\"title\"><p id=\"world\">Hello world22</p><span>span123</span></h2>"
  h2 <- query "h2" che
  p <- find "p" h2
  t <- attr "id" p
  putStrLn $ show t