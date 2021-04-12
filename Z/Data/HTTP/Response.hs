module Z.Data.HTTP.Response where

import Data.Word
import qualified Z.Data.ASCII as C
import qualified Z.Data.Builder as B
import Z.Data.HTTP.Internal
import qualified Z.Data.Parser as P
import qualified Z.Data.Vector as V
import Z.HTTP.Internal
import Z.IO

data Response = Response
  { responseVersion :: !HTTPVersion,
    responseCode :: !StatusCode,
    responseMessage :: !StatusMessage,
    responseHeaders :: Headers,
    responseBody :: Body
  }

data StatusCode
  = Informational Informational
  | Success Success
  | Redirection Redirection
  | ClientError ClientError
  | ServerError ServerError

type Informational = Word16

type Success = Word16

type Redirection = Word16

type ClientError = Word16

type ServerError = Word16

statusCodeToBytes :: StatusCode -> V.Bytes
statusCodeToBytes (Informational n) = (B.build . B.int) n
statusCodeToBytes (Success n) = (B.build . B.int) n
statusCodeToBytes (Redirection n) = (B.build . B.int) n
statusCodeToBytes (ClientError n) = (B.build . B.int) n
statusCodeToBytes (ServerError n) = (B.build . B.int) n

type StatusMessage = V.Bytes

codeToMessage :: StatusCode -> StatusMessage
codeToMessage = error "todo"

parseResponse :: HasCallStack => V.Bytes -> Response
parseResponse a = undefined parser

parser :: P.Parser Response
parser = do
  version <- P.takeWhile (/= C.SPACE)
  P.skipWord8
  statusCode <- P.takeWhile (/= C.SPACE)
  P.skipWord8
  statusMessage <- P.takeWhile (/= C.CARRIAGE_RETURN)
  P.skipWord8 >> P.skipWord8
  undefined
  pure $ Response (undefined parseVersion version) (undefined parseStatusCode statusCode) statusMessage (undefined undefined undefined) undefined

classifyStatusCode' :: Word16 -> Informational -> StatusCode
classifyStatusCode' a = case a of
  1 -> Informational
  2 -> Success
  3 -> Redirection
  4 -> ClientError
  5 -> ServerError
  _ -> calculated

parseStatusCode :: P.Parser StatusCode
parseStatusCode = do
  a <- P.int @Word16
  b <- P.int
  c <- P.int
  pure $ classifyStatusCode' a (a * 100 + b * 10 + c)

responseToBytes :: Response -> V.Bytes
responseToBytes a = mconcat [version, SPACE, statusCode, SPACE, statusMessage, CRLF, responseBody a, CRLF]
  where
    version = versionToBytes $ responseVersion a
    statusCode = statusCodeToBytes $ responseCode a
    statusMessage = responseMessage a
