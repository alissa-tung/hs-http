module Z.HTTP.Client where

import qualified Z.Data.Builder as B
import Z.Data.CBytes
import Z.Data.HTTP
import Z.Data.HTTP.Request
import Z.Data.HTTP.Response
import qualified Z.Data.Text as T
import Z.IO
import Z.IO.Internal
import Z.IO.Network

headersOnly :: BufferedOutput -> IO ()
headersOnly _ = pure ()

resolveDNS :: Host -> Maybe AddrInfo -> IO AddrInfo
resolveDNS (hostName, portNumber) hints = head <$> getAddrInfo hints hostName (cast portNumber)
  where
    cast :: PortNumber -> CBytes = buildCBytes . B.int

defaultHintsHTTP :: Maybe AddrInfo
defaultHintsHTTP = Just $ defaultHints {addrFlags = [AI_NUMERICSERV], addrSocketType = SOCK_STREAM}

httpClient :: IODev dev => dev -> IO HTTPConnection
httpClient tcp = do
  (i, o) <- newBufferedIO tcp
  pure $ HTTPConnection i o

httpClient' :: (Input i, Output o) => i -> o -> Int -> Int -> IO HTTPConnection
httpClient' i o inSize outSize = do
  bi <- newBufferedInput' inSize i
  bo <- newBufferedOutput' outSize o
  pure $ HTTPConnection bi bo

sendRequest :: HTTPConnection -> Request -> IO ()
sendRequest c q = writeBuffer' (httpClientOut c) (requestToBytes q)

receiveResponse :: HTTPConnection -> IO Response
receiveResponse c = do
  o <- readBuffer (httpClientIn c)
  return $ parseResponse o

withResponse :: HTTPConnection -> (Response -> IO b) -> IO b
withResponse c = (>>=) (receiveResponse c)
