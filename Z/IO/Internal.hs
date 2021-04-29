module Z.IO.Internal where

import qualified Z.Data.Vector as V
import Z.IO
import Z.IO.Network

-- newBufferedIO :: IODev dev => dev -> IO (BufferedInput, BufferedOutput)
-- {-# INLINE newBufferedIO #-}
-- newBufferedIO dev = newBufferedIO' dev defaultChunkSize defaultChunkSize

-- newBufferedIO' :: IODev dev => dev -> Int -> Int -> IO (BufferedInput, BufferedOutput)
-- newBufferedIO' dev inSize outSize = do
--   i <- newBufferedInput' inSize dev
--   o <- newBufferedOutput' outSize dev
--   pure (i, o)

-- writeBuffer' :: BufferedOutput -> V.Bytes -> IO ()
-- writeBuffer' o = flip (>>) (flushBuffer o) . writeBuffer o

-- type IODev dev = (Input dev, Output dev)
