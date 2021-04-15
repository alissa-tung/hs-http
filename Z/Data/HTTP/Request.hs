module Z.Data.HTTP.Request where

import Control.Monad.State
import qualified Z.Data.ASCII as C
import qualified Z.Data.Builder as B
import Z.Data.HTTP
import Z.Data.HTTP.Internal
import qualified Z.Data.Parser as P
import qualified Z.Data.Text as T
import qualified Z.Data.Vector as V

data Request = Request
  { requestMethod :: !Method,
    requestPath :: !Path,
    requestVersion :: !HTTPVersion,
    requestHeaders :: Headers,
    requestBody :: Body
  }

emptyRequest :: Request
emptyRequest = Request V.empty V.empty HTTP1_1 V.empty V.empty

type Method = V.Bytes

pattern GET :: Method
pattern GET = "GET"

type Path = V.Bytes

newtype RequestBuilder a = RequestBuilder (State Request a)
  deriving (Functor, Applicative, Monad, MonadState Request)

buildRequest :: RequestBuilder a -> Request
buildRequest (RequestBuilder s) = execState s emptyRequest

buildRequestM :: Monad m => RequestBuilder a -> m Request
{-# INLINE buildRequestM #-}
buildRequestM = pure . buildRequest

consHeader :: HeaderHeader -> HeaderValue -> Headers -> Headers
consHeader header value = V.cons (header, value)

snocHeader :: Headers -> HeaderHeader -> HeaderValue -> Headers
snocHeader headers header value = V.snoc headers (header, value)

setHost :: Host -> RequestBuilder ()
setHost host = do
  q <- get
  put q {requestHeaders = consHeader "Host" (hostToBytes host) (requestHeaders q)}

setHeader :: HeaderHeader -> HeaderValue -> RequestBuilder ()
setHeader h v = do
  q <- get
  put q {requestHeaders = snocHeader (requestHeaders q) h v}

http :: Method -> Path -> HTTPVersion -> RequestBuilder ()
http m p v = do
  q <- get
  put q {requestMethod = m, requestPath = p, requestVersion = v}

pattern AcceptEncoding :: HeaderHeader
pattern AcceptEncoding = "Accept-Encoding"

type EncodingAlgo = HeaderValue

pattern GZIP :: EncodingAlgo
pattern GZIP = "gzip"

pattern AcceptLanguage :: HeaderHeader
pattern AcceptLanguage = "Accept-Language"

type Lang = V.Bytes

pattern LANG_FR :: Lang
pattern LANG_FR = "fr"

requestToBytes :: Request -> V.Bytes
requestToBytes q = mconcat [method, SPACE, path, SPACE, version, CRLF, headers, body, CRLF]
  where
    method :: V.Bytes = requestMethod q
    path :: V.Bytes = requestPath q
    version :: V.Bytes = versionToBytes $ requestVersion q
    headers :: V.Bytes = headersToBytes $ requestHeaders q
    body :: V.Bytes = requestBody q

debugShowRequest :: Request -> String
debugShowRequest = map C.w2c . (V.unpack . requestToBytes)
