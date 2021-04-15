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

{-
data StatusCode
  = Informational Informational
  | Success Success
  | Redirection Redirection
  | ClientError ClientError
  | ServerError ServerError
-}

newtype StatusCode = StatusCode Word16
  deriving newtype (Num, Show, Print)

type Informational = Word16

type Success = Word16

type Redirection = Word16

type ClientError = Word16

type ServerError = Word16

-- statusCodeToBytes :: StatusCode -> V.Bytes
-- statusCodeToBytes (Informational n) = (B.build . B.int) n
-- statusCodeToBytes (Success n) = (B.build . B.int) n
-- statusCodeToBytes (Redirection n) = (B.build . B.int) n
-- statusCodeToBytes (ClientError n) = (B.build . B.int) n
-- statusCodeToBytes (ServerError n) = (B.build . B.int) n

type StatusMessage = V.Bytes

statusCodeToBytes :: StatusCode -> V.Bytes
statusCodeToBytes (StatusCode n) = (B.build . B.int) n

{-
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
-}

responseToBytes :: Response -> V.Bytes
responseToBytes a = mconcat [version, SPACE, statusCode, SPACE, statusMessage, CRLF, responseBody a, CRLF]
  where
    version = versionToBytes $ responseVersion a
    statusCode = statusCodeToBytes $ responseCode a
    statusMessage = responseMessage a

parseResponse :: V.Bytes -> IO Response
parseResponse = unwrap T.empty . P.parse' responseParser

responseParser :: P.Parser Response
responseParser = do
  P.bytes "HTTP/"
  fstVerNum <- P.peek
  c <- P.peek
  sndVerNum <- case c of
    C.SPACE -> undefined
    C.DOT -> P.peek
  P.skipSpaces

  aCode <- P.peek
  bCode <- P.peek
  cCode <- P.peek
  let code = parseStatusCode (aCode, bCode, cCode)
  P.skipSpaces

  msg <- P.takeWhile (/= C.CARRIAGE_RETURN)
  P.skipWord8 >> P.skipWord8

  let readLoop = do
        (txt, eof) <- undefined
        if eof
          then pure txt
          else undefined
  readLoop

  pure $ Response undefined code msg undefined undefined
  where
    h = do
      c <- P.peekMaybe
      case c of
        Nothing -> pure undefined
        Just x -> undefined
      undefined

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
