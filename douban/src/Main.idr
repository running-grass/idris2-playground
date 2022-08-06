module Main

import Network.HTTP
import Network.HTTP.URL
import Utils.String
import System.File
import System.File.Mode
import Data.Nat
import Control.Monad.Error.Either
import Control.Monad.Error.Interface


with_client : {e : _} -> IO (HttpClient e) -> (HttpClient e -> EitherT (HttpError e) IO a) -> EitherT (HttpError e) IO a
with_client client f = MkEitherT $ do
  c <- client
  Right ok <- runEitherT (f c)
  | Left err => close c *> pure (Left err)
  close c
  pure (Right ok)

map_error : Functor m => (e -> e') -> EitherT e m a -> EitherT e' m a
map_error f = bimapEitherT f id

export
test_redirect : EitherT String IO ()
test_redirect = map_error show $ with_client {e=()} new_client_default $ \client => do
  putStrLn "sending request"
  (response, content) <- request client GET (url' "https://apache.org/") [] ()
  putStrLn "response header received"
  printLn response
  putStrLn "downloading response"
  content <- toList_ content
  printLn $ utf8_pack $ content


main : IO (Either String ())
main = runEitherT test_redirect