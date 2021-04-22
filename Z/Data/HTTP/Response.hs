module Z.Data.HTTP.Response where

import Control.Applicative
import Data.Word
import qualified Z.Data.ASCII as C
import qualified Z.Data.Builder as B
import Z.Data.HTTP.Internal
import Z.Data.Internal
import qualified Z.Data.Parser as P
import Z.Data.Text as T
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

data ResponseExt = ResponseExt
  { responseResponse :: Response,
    payloadHeader :: HeaderPayload
  }

newtype StatusCode = StatusCode Word16
  deriving newtype (Num, Show, Print)

type Informational = Word16

type Success = Word16

type Redirection = Word16

type ClientError = Word16

type ServerError = Word16

type StatusMessage = V.Bytes

statusCodeToBytes :: StatusCode -> V.Bytes
statusCodeToBytes (StatusCode n) = (B.build . B.int) n

responseToBytes :: Response -> V.Bytes
responseToBytes a = mconcat [version, SPACE, statusCode, SPACE, statusMessage, CRLF, headersToBytes headers, responseBody a, CRLF]
  where
    version = versionToBytes $ responseVersion a
    statusCode = statusCodeToBytes $ responseCode a
    statusMessage = responseMessage a
    headers = responseHeaders a

parseStatusCode :: (Word8, Word8, Word8) -> StatusCode
parseStatusCode (a, b, c) =
  let (a', b', c') = (word8ToWord16 a, word8ToWord16 b, word8ToWord16 c)
   in StatusCode (a' * 100 + b' * 10 + c')

parseHeaderLn :: P.Parser Header
parseHeaderLn = do
  header <- P.takeWhile (/= C.COLON)
  P.skipWord8 >> P.skipWord8
  value <- P.takeWhile (/= C.CARRIAGE_RETURN)
  pSkipCRLF
  pure (header, value)

takeBody :: P.Parser Body
takeBody = undefined

word8ToWord16 :: Word8 -> Word16
word8ToWord16 = \case
  C.DIGIT_0 -> 0
  C.DIGIT_1 -> 1
  C.DIGIT_2 -> 2
  C.DIGIT_3 -> 3
  C.DIGIT_4 -> 4
  C.DIGIT_5 -> 5
  C.DIGIT_6 -> 6
  C.DIGIT_7 -> 7
  C.DIGIT_8 -> 8
  C.DIGIT_9 -> 9
  _ -> undefined
