module Z.Data.HTTP.Response.Parser where

import Data.Functor
import qualified Z.Data.ASCII as C
import qualified Z.Data.Builder as B
import Z.Data.HTTP.Internal
import Z.Data.HTTP.Response
import Z.Data.Internal
import qualified Z.Data.Parser as P
import qualified Z.Data.Text as T
import qualified Z.Data.Vector as V
import Z.IO

parseResponse :: HasCallStack => V.Bytes -> IO Response
parseResponse txt = do
  let (body, res) = P.parse responseParser txt
  (ver, code, msg, hs) <- unwrap T.empty res
  pure $ Response ver code msg hs body

responseParser :: HasCallStack => P.Parser (HTTPVersion, StatusCode, StatusMessage, Headers)
responseParser = do
  P.bytes "HTTP/"
  m <- peekNum
  P.word8 C.DOT
  n <- peekNum
  P.skipSpaces

  a <- peekNum
  b <- peekNum
  c <- peekNum
  let code = cast a * 100 + cast b * 10 + cast c
  P.skipSpaces

  msg <- P.takeWhile (/= C.CARRIAGE_RETURN)
  P.word8 C.CARRIAGE_RETURN *> P.word8 C.NEWLINE

  let loopRead (acc :: Headers) = do
        c <- P.peekMaybe
        case c of
          Nothing -> pure acc
          Just C.CARRIAGE_RETURN -> P.word8 C.CARRIAGE_RETURN $> P.word8 C.NEWLINE $> acc
          Just c -> do
            txtLn <- readHeaderLn
            loopRead (parseHeader txtLn `V.cons` acc)
  hs <- loopRead V.empty

  pure
    (if m == C.DIGIT_1 && n == C.DIGIT_1 then HTTP1_1 else undefined, code, msg, hs)
  where
    peekNum = P.satisfy C.isDigit
    cast n = fromIntegral $ n - C.DIGIT_0
    readHeaderLn = do
      ret <- P.takeWhile (/= C.CARRIAGE_RETURN)
      P.word8 C.CARRIAGE_RETURN *> P.word8 C.NEWLINE
      pure ret
    parseHeader :: V.Bytes -> Header
    parseHeader txt = case snd (P.parse headerParser txt) of
      Left err -> error $ show err
      Right ok -> ok
    headerParser = do
      header <- P.takeWhile (/= C.COLON)
      P.word8 C.COLON *> P.skipSpaces
      value <- P.takeWhile (/= C.CARRIAGE_RETURN)
      pure (header, value)

testResponse :: V.Bytes
testResponse = "HTTP/1.1 200 OK\r\nDate: Sat, 09 Oct 2010 14:28:02 GMT\r\nServer: Apache\r\nLast-Modified: Tue, 01 Dec 2009 20:18:22 GMT\r\nETag: \"51142bc1-7449-479b075b2891b\"\r\nAccept-Ranges: bytes\r\nContent-Length: 29769\r\nContent-Type: text/html\r\n\r\n<!DOCTYPE html... (here comes the 29769 bytes of the requested web page)"

debugMain :: IO ()
debugMain = do
  res <- parseResponse testResponse
  putStrLn (debugShow $ responseToBytes res)
