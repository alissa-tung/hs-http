module Z.Data.HTTP.Response.Parser where

import GHC.Word
import qualified Z.Data.ASCII as C
import Z.Data.CBytes
import Z.Data.HTTP.Internal
import Z.Data.HTTP.Response
import qualified Z.Data.Parser as P
import qualified Z.Data.Text as T
import qualified Z.Data.Vector as V
import Z.IO.Exception

parse :: HasCallStack => V.Bytes -> IO Response
parse = unwrap T.empty . P.parse' parser

parser :: HasCallStack => P.Parser Response
parser = do
  P.bytes "HTTP/"
  fstVerNum <- P.satisfy C.isDigit
  P.word8 C.DOT
  sndVerNum <- P.satisfy C.isDigit
  P.skipSpaces

  aCode <- P.satisfy C.isDigit
  bCode <- P.satisfy C.isDigit
  cCode <- P.satisfy C.isDigit
  P.skipSpaces

  msg <- P.takeWhile (/= C.CARRIAGE_RETURN)
  P.word8 C.CARRIAGE_RETURN *> P.word8 C.NEWLINE

  headers :: Headers <- V.pack . fst <$> readHeaderLn

  let loopRead acc = do
        c <- P.peekMaybe
        case c of
          Nothing -> pure (acc, True)
          Just c -> loopRead $ c : acc
  body :: Body <- V.pack <$> (fst <$> loopRead [])

  pure $
    Response
      (if fstVerNum == C.DIGIT_1 && sndVerNum == C.DIGIT_1 then HTTP1_1 else undefined)
      (cast aCode * 100 + cast bCode * 10 + cast cCode)
      msg
      headers
      body
  where
    cast n = fromIntegral $ n - C.DIGIT_0
    readHeaderLn :: P.Parser ([Header], Bool) =
      let loopRead acc = do
            h <- P.peekMaybe
            case h of
              Nothing -> return (acc, True)
              Just h -> do
                eader <- P.takeWhile (/= C.COLON)
                P.word8 C.COLON *> P.skipSpaces
                let header = V.cons h eader
                value <- P.takeWhile (/= C.CARRIAGE_RETURN)
                P.word8 C.CARRIAGE_RETURN *> P.word8 C.NEWLINE
                loopRead $ (header, value) : acc
       in loopRead []

testResponse :: V.Bytes
testResponse = "HTTP/1.1 200 OK\r\nDate: Sat, 09 Oct 2010 14:28:02 GMT\r\nServer: Apache\r\nLast-Modified: Tue, 01 Dec 2009 20:18:22 GMT\r\nETag: \"51142bc1-7449-479b075b2891b\"\r\nAccept-Ranges: bytes\r\nContent-Length: 29769\r\nContent-Type: text/html\r\n<!DOCTYPE html... (here comes the 29769 bytes of the requested web page)"
