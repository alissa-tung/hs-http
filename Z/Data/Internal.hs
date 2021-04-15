module Z.Data.Internal where

import qualified Z.Data.ASCII as C
import qualified Z.Data.Vector as V

data HTTPRequestException
  = BadHeaderLn
  | NoHostHeaders
  deriving (Show)

debugShow :: V.Bytes -> String
debugShow = map C.w2c . V.unpack

debugShow' :: String -> String
debugShow' = undefined
