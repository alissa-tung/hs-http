module Main where

import Data.Functor
import Z.Data.HTTP
import Z.Data.HTTP.Internal
import Z.Data.HTTP.Request as Q
import Z.Data.HTTP.Response
import qualified Z.Data.Text as T
import qualified Z.Data.Vector as V
import Z.HTTP.Client
import Z.HTTP.Internal
import Z.IO
import Z.IO.Internal
import Z.IO.Network

main :: IO ()
main = do
  addr <- resolveDNS mozHost defaultHintsHTTP
  withResource (initTCPClient defaultTCPClientConfig {tcpRemoteAddr = addrAddress addr}) $ \tcp -> do
    (i, o) <- newBufferedIO tcp
    writeBuffer' o $ requestToBytes (buildRequest $ Q.http GET "/" HTTP1_1 >> setHost mozHost >> setHeader AcceptLanguage LANG_FR)
    readBuffer i >>= print . T.validate
  where
    mozHost :: Host = ("developer.mozilla.org", defaultPortHTTP)

readme :: IO ()
readme = do
  -- Resolve DNS and start a TCP client for the HTTP connection.
  -- The client is guaranteed to be closed automatically after all tasks were done.
  addr <- resolveDNS mozHost defaultHintsHTTP
  withResource (initTCPClient defaultTCPClientConfig {tcpRemoteAddr = addrAddress addr}) $ \tcp -> do
    -- Start a HTTP client.
    c <- httpClient tcp
    -- Build a simple HTTP request.
    let q = buildRequest $ do
          Q.http GET "/" HTTP1_1
          setHost mozHost
          setHeader AcceptLanguage LANG_FR
    -- Send the request.
    sendRequest c q
    -- Receive a response then perform some actions using the response.
    withResponse c $ \a -> printStdLn (responseToBytes a)
  where
    mozHost :: Host = ("developer.mozilla.org", defaultPortHTTP)
