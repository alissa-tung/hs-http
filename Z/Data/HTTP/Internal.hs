module Z.Data.HTTP.Internal where

import Data.Maybe
import qualified Z.Data.ASCII as C
import qualified Z.Data.Builder as B
import qualified Z.Data.Parser as P
import qualified Z.Data.Vector as V

data HTTPVersion = HTTP1_1

versionToBytes :: HTTPVersion -> V.Bytes
versionToBytes = \case
  HTTP1_1 -> "HTTP/1.1"

parseVersion :: P.Parser HTTPVersion
parseVersion = do
  P.skipWhile (/= C.LETTER_P) >> P.skipWord8
  c0 <- P.peek
  P.skipWord8
  c1 <- P.peekMaybe
  let c1' = fromMaybe (error "todo") c1
  return (if c0 == C.DIGIT_1 && c1' == C.DIGIT_1 then HTTP1_1 else error "todo")

type Headers :: *

type Headers = (V.Vector (HeaderHeader, HeaderValue))

headersToBytes' :: Headers -> V.Bytes
headersToBytes' hs = case V.unpack hs of
  [] -> V.empty
  h : hs -> headerToBytes h <> headersToBytes' hs
  where
    headersToBytes' :: [(HeaderHeader, HeaderValue)] -> V.Bytes = \case
      [] -> V.empty
      h : hs -> headerToBytes h <> headersToBytes' hs

headersToBytes :: Headers -> V.Bytes
headersToBytes hs = let hs' = V.unpack hs in mconcat (map headerToBytes hs')

type Header = (HeaderHeader, HeaderValue)

data HeaderPayload
  = HTransferEncoding [TransferCoding]
  | -- | Content-Length = 1*DIGIT
    HContentLength Int -- readExactly :: HasCallStack => Int -> BufferedInput -> IO V.Bytes

data MessageBodyHeader = TransferEncoding | ContentLength Int

type MessageBody =
  -- | message-body = *OCTET
  V.Bytes

data TransferCoding
  = Chunked
  | Compress
  | Deflate
  | GZIP
  | TransferExtension

data TransferParameter

headerToBytes :: Header -> V.Bytes
headerToBytes (header, value) = header <> ": " <> value <> CRLF

type HeaderHeader = V.Bytes

type HeaderValue = V.Bytes

joinHeaders :: Headers -> V.Bytes
joinHeaders hs = (V.concat . V.unpack) (fmap (\(header, value) -> header <> ": " <> value) hs)

emptyHeaders :: Headers
emptyHeaders = V.empty

type Body = V.Bytes

emptyBody :: Body
emptyBody = V.empty

data LinearWhitespace = OWS | RWS | BWS

data TokenChar

pattern CRLF :: V.Bytes
pattern CRLF = "\r\n"

pSkipCRLF :: P.Parser ()
pSkipCRLF = P.word8 C.CARRIAGE_RETURN >> P.word8 C.NEWLINE

pattern SPACE :: V.Bytes
pattern SPACE = " "
