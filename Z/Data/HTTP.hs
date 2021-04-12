module Z.Data.HTTP where

import qualified Z.Data.Builder as B
import Z.Data.CBytes
import qualified Z.Data.Vector as V
import Z.IO
import Z.IO.Network

type Host = (HostName, PortNumber)

hostToBytes :: Host -> V.Bytes
hostToBytes (hostName, portNumber) = "Host: " <> toBytes hostName <> ":" <> B.build (B.int portNumber)

defaultPortHTTP :: PortNumber
defaultPortHTTP = 80

data HTTPConnection = HTTPConnection
  { httpClientIn :: BufferedInput,
    httpClientOut :: BufferedOutput
  }
