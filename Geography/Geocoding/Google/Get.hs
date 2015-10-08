-- | Some handy functions for retrieving a web-page.
module Geography.Geocoding.Google.Get (get, maybeGet, eitherGet, timeoutGet) where

import Data.Char (intToDigit)
import Control.Concurrent ( threadDelay, newEmptyMVar, myThreadId
                          , forkIO, putMVar, killThread, takeMVar )
import Network.HTTP ( simpleHTTP, insertHeaders, Header (..), HeaderName (..)
                    , Request (..), RequestMethod (..), rspBody )
import Network.URI (parseURI, URI)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, SomeException)

err :: String -> IO a
err msg = do
  hPutStrLn stderr msg
  exitFailure

get :: URI -> IO String
get uri = do
  eresp <- simpleHTTP $ insertHeaders [Header HdrAccept "*/*"]
                                      (Request uri GET [] "")
  case eresp of
    Left er -> fail $ show er
    Right res -> return $ rspBody res

maybeGet :: URI -> IO (Maybe String)
maybeGet uri = either (const Nothing) Just `fmap` eitherGet uri

eitherGet :: URI -> IO (Either String String)
eitherGet uri = timeoutGet uri

-- 5 sec
waitTimeout = 10000000

timeoutGet :: URI -> IO (Either String String)
timeoutGet uri = do
  mv   <- newEmptyMVar
  mid  <- myThreadId
  tid1 <- forkIO $ do
    x <- get uri 
    putMVar mv $ Right x
    `catch` (\ e -> putMVar mv . Left . show $ (e :: SomeException))
  tid2 <- forkIO $ do
    threadDelay waitTimeout 
    killThread tid1 
    putMVar mv (Left "timeout")
  takeMVar mv

