module Z.Data.HTTP.Types where

import Data.CaseInsensitive (CI)
import Data.String (IsString)
import qualified Z.Data.ASCII as C
import Z.Data.HTTP.Types.Internal
import qualified Z.Data.Vector as V

-- | Message Format
-- All HTTP/1.1 messages consist of a start-line followed by a sequence of
-- octets in a format similar to the Internet Message Format: zero or more
-- header fields (collectively referred to as the "headers" or the "header
-- section"), an empty line indicating the end of the header section, and an
-- optional message body.
-- See https://datatracker.ietf.org/doc/html/rfc7230#section-3.
data HTTPMessage = HTTPMessage
  { startLine :: StartLine,
    headerFields :: HeaderFields,
    messageBody :: Maybe MessageBody
  }

-- | Start Line
-- An HTTP message can be either a request from client to server or a response from server to client.
-- See https://datatracker.ietf.org/doc/html/rfc7230#section-3.1.
data StartLine = Either RequestLine StatusLine

-- | Request Line
-- A request-line begins with a method token, followed by a single space (SP),
-- the request-target, another single space (SP), the protocol version, and
-- ends with CRLF.
-- See https://datatracker.ietf.org/doc/html/rfc7230#section-3.1.1.
data RequestLine = RequestLine
  { requestMethod :: Method,
    requestTarget :: RequestTarget,
    requestHTTPVersion :: HTTPVersion
  }

-- | Method Token
-- The method token indicates the request method to be performed on the target resource.
-- The request method is case-sensitive. By convention, standardized methods are defined in all-uppercase US-ASCII letters.
-- See https://datatracker.ietf.org/doc/html/rfc7231#section-4.
newtype Method = Method (CI V.Bytes)
  deriving (IsString)

-- | Request Target
-- Once an inbound connection is obtained, the client sends an HTTP request
-- message with a request-target derived from the target URI.
-- There are four distinct formats for the request-target, depending on both
-- the method being requested and whether the request is to a proxy.
-- See https://datatracker.ietf.org/doc/html/rfc7230#section-5.3.
data RequestTarget = ROriginForm OriginForm | RAbsoluteForm AbsoluteForm | RAuthorityForm AuthorityForm | RAsteriskForm AsteriskForm

data PathChar = PUnreservedChar UnreservedChar | PPctEncoded PctEncoded | PSubDelims SubDelims

pattern PColon :: PathChar
pattern PColon = C.COLON

pattern PAt :: PathChar
pattern PAt = C.AT

data OriginForm = OriginForm AbsolutePath Maybe Query

newtype AbsolutePath = RawAbsolutePath V.Vector Segment

newtype Query = Query V.Vector QueryChar

data QueryChar = QPathChar PathChar | QSlash | QQuestion

mkAbsolutePath :: V.Vector Segment -> AbsolutePath
mkAbsolutePath xs = if V.length xs >= 1 then RawAbsolutePath xs else undefined

newtype Segment = Segment V.Vector PathChar

-- | Message Body
-- The message body (if any) of an HTTP message is used to carry the payload
-- body of that request or response.
-- See https://datatracker.ietf.org/doc/html/rfc7230#section-3.3.
-- The presence of a message body in a request is signaled by a Content-Length or Transfer-Encoding header field.
-- The presence of a message body in a response depends on both the request method to which it is responding and the response status code.
newtype MessageBody = MessageBody V.Bytes
  deriving (IsString)
