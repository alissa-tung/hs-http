module Z.Data.HTTP.Response.Parser where

import Data.Bifunctor
import Data.Functor
import GHC.IO
import GHC.Word
import qualified Z.Data.ASCII as C
import qualified Z.Data.Builder as B
import Z.Data.CBytes
import Z.Data.HTTP.Internal
import Z.Data.HTTP.Response
import Z.Data.Internal
import qualified Z.Data.Parser as P
import qualified Z.Data.Text as T
import qualified Z.Data.Vector as V
import Z.IO
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
testResponse = "HTTP/1.1 200 OK\r\nDate: Sat, 09 Oct 2010 14:28:02 GMT\r\nServer: Apache\r\nLast-Modified: Tue, 01 Dec 2009 20:18:22 GMT\r\nETag: \"51142bc1-7449-479b075b2891b\"\r\nAccept-Ranges: bytes\r\nContent-Length: 29769\r\nContent-Type: text/html\r\n\r\n<!DOCTYPE html... (here comes the 29769 bytes of the requested web page)"

testParser :: P.Parser (HTTPVersion, Int, StatusMessage, Headers)
testParser = do
  P.bytes "HTTP/"
  fstVerNum <- peekNum
  P.word8 C.DOT
  sndVerNum <- peekNum
  P.skipSpaces

  aCode <- peekNum
  bCode <- peekNum
  cCode <- peekNum
  let code = cast aCode * 100 + cast bCode * 10 + cast cCode
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
            pure $! unsafePerformIO (putStrLn $ debugShow $ "[DEBUG]: " <> c `V.cons` "\n")
            pure $! unsafePerformIO (putStrLn $ debugShow $ "[DEBUG]: " <> txtLn <> "\n")
            loopRead (parseHeader txtLn `V.cons` acc)
  hs <- loopRead V.empty

  pure
    ( if fstVerNum == C.DIGIT_1 && sndVerNum == C.DIGIT_1
        then HTTP1_1
        else undefined,
      code,
      msg,
      hs
    )
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
      P.word8 C.CARRIAGE_RETURN *> P.word8 C.NEWLINE
      pure (header, value)

debugMain :: IO ()
debugMain = do
  let res = P.parse testParser testResponse
  (r, n, msg, hs) <- unwrap T.empty (snd res)
  let l = fst res
  let r' = versionToBytes r
  putStrLn . debugShow $
    "The result HTTP version is: " <> versionToBytes r <> ";\n"
      <> "The result status code is: "
      <> B.build (B.int n)
      <> ";\n"
      <> "The result status message is: "
      <> msg
      <> ";\n"
      <> "\n"
      -- <> headersToBytes hs
      <> "\n"
      <> "Left with:\n"
      <> l

notMain' :: String
notMain' = debugShow $ V.pack [10]

notMain :: String
notMain = debugShow $ V.pack [13, 10, 60, 33, 68, 79, 67, 84]

testHeader :: V.Bytes
testHeader = "Date: Sat, 09 Oct 2010 14:28:02 GMT\r\nServer: Apache\r\n"

testSingl :: V.Bytes
testSingl = "Date: Sat, 09 Oct 2010 14:28:02 GMT\r\n"

testHeaderParser :: P.Parser Header
testHeaderParser = do
  header <- P.takeWhile (/= C.COLON)
  P.word8 C.COLON *> P.skipSpaces
  value <- P.takeWhile (/= C.CARRIAGE_RETURN)
  P.word8 C.CARRIAGE_RETURN *> P.word8 C.NEWLINE
  pure (header, value)

debugParse :: (V.Bytes, Either err Header) -> IO ()
debugParse res =
  putStrLn $
    debugShow (fst res) <> "\n"
      <> debugShow
        ( case snd res of
            Left err -> undefined
            Right ok -> headerToBytes ok
        )

parseGrp :: V.Vector Header -> P.Parser Headers
parseGrp acc = do
  c <- P.peekMaybe
  case c of
    Nothing -> pure acc
    Just C.CARRIAGE_RETURN -> P.word8 C.CARRIAGE_RETURN $> P.word8 C.NEWLINE $> acc
    Just c -> do
      txtLn <- readHeaderLn
      let curLn = c `V.cons` txtLn
      parseGrp (parseHeader curLn `V.cons` acc)
  where
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
      P.word8 C.CARRIAGE_RETURN *> P.word8 C.NEWLINE
      pure (header, value)

debugGrp :: IO ()
debugGrp = debugParse' $ P.parse (parseGrp V.empty) testHeader

debugParse' :: (V.Bytes, Either P.ParseError Headers) -> IO ()
debugParse' x =
  putStrLn $
    debugShow (fst x)
      <> debugShow
        ( case snd x of
            Left err -> undefined
            Right ok -> headersToBytes ok
        )
