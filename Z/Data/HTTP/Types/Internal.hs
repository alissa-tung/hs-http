module Z.Data.HTTP.Types.Internal
  ( UnreservedChar,
    mkUnreservedChar,
    PctEncoded,
    mkPctEncoded,
    SubDelims,
    mkSubDelims,
    isALPHA,
  )
where

import Data.Word (Word8)
import qualified Z.Data.ASCII as C

-- | uppercase and lowercase letters
isALPHA :: Word8 -> Bool
isALPHA c = C.isUpper c && C.isLower c

-- | Unreserved Characters
-- Characters that are allowed in a URI but do not have a reserved purpose are called unreserved.
-- These include uppercase and lowercase letters, decimal digits, hyphen, period, underscore, and tilde.
-- See https://datatracker.ietf.org/doc/html/rfc3986#section-2.3.
newtype UnreservedChar = RawUnreservedChar Word8

mkUnreservedChar :: Word8 -> UnreservedChar
mkUnreservedChar c =
  if isALPHA c
    || C.isDigit c
    || c == C.HYPHEN
    || c == C.PERIOD
    || c == C.UNDERSCORE
    || c == C.TILDE
    then RawUnreservedChar c
    else undefined

-- | pct-encoded = "%" HEXDIG HEXDIG
data PctEncoded = RawPctEncoded Word8 Word8

mkPctEncoded :: Word8 -> Word8 -> PctEncoded
mkPctEncoded x y =
  if C.isDigit x && C.isDigit y
    then RawPctEncoded x y
    else undefined

newtype SubDelims = RawSubDelims Word8

mkSubDelims :: Word8 -> SubDelims
mkSubDelims c =
  if c == C.EXCLAM
    || c == C.DOLLAR
    || c == C.AND
    || c == C.SINGLE_QUOTE
    || c == C.PAREN_LEFT
    || c == C.PAREN_RIGHT
    || c == C.ASTERISK
    || c == C.PLUS
    || c == C.COMMA
    || c == C.SEMICOLON
    || c == C.EQUAL
    then RawSubDelims c
    else undefined
