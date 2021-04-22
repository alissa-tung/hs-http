# `hs-http`

## Development

```sh
git clone git@github.com:alissa-tung/hs-http.git
cd ./hs-http
cabal update && cabal build
```

### Client

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Z.HTTP.Client
import Z.Data.HTTP.Request as Q
```

```haskell
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
    withResponse c $ \a -> putStrLn $ debugShow (responseToBytes a)
  where
    mozHost :: Host = ("developer.mozilla.org", defaultPortHTTP)
```
