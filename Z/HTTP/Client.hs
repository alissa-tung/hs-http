module Z.HTTP.Client where

import qualified Z.Data.Builder as B
import Z.Data.CBytes
import Z.Data.HTTP
import Z.Data.HTTP.Request
import Z.Data.HTTP.Response
import Z.Data.HTTP.Response.Parser
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

httpClient :: IODev dev => dev -> IO HTTPClient
httpClient tcp = do
  (i, o) <- newBufferedIO tcp
  pure $ HTTPClient i o

httpClient' :: (Input i, Output o) => i -> o -> Int -> Int -> IO HTTPClient
httpClient' i o inSize outSize = do
  bi <- newBufferedInput' inSize i
  bo <- newBufferedOutput' outSize o
  pure $ HTTPClient bi bo

sendRequest :: HTTPClient -> Request -> IO ()
sendRequest c q = writeBuffer' (httpClientOut c) (requestToBytes q)

receiveResponse :: HTTPClient -> IO Response
receiveResponse c = do
  o <- readBuffer (httpClientIn c)
  parseResponse o

withResponse :: HTTPClient -> (Response -> IO b) -> IO b
withResponse c = (>>=) (receiveResponse c)
