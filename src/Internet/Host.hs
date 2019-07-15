{-# language BangPatterns #-}
{-# language LambdaCase #-}
{-# language MultiWayIf #-}
{-# language ScopedTypeVariables #-}
{-# language MagicHash #-}

module Internet.Host
  ( -- * Types
    Hostname(..)
    -- * Decoding
  , lenientDecodeString
  , decodeUtf8
    -- * Special hosts
  , root
  ) where

import Control.Monad.ST (ST,runST)
import Data.Bytes.Types (Bytes(..),MutableBytes(..))
import Data.Word (Word8)
import Data.Char (ord)
import GHC.Exts (IsString)
import Data.Primitive (MutableByteArray(..),MutablePrimArray(..))
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

-- | An Internet hostname as described by
-- <https://tools.ietf.org/html/rfc1123 RFC-1123>.
-- Such a hostname is used by DNS, but it need not be fully qualified.
-- The allowed characters ranges are @a-z@ (case insensitive), @0-9@,
-- and hyphen. To support efficient equality testing and comparison,
-- all hostnames are canonized to lower case.
newtype Hostname = Hostname Bytes
  deriving (Eq,Ord)

-- | Lenient in decoding the hostname. This never throws an exception.
instance IsString Hostname where
  fromString = lenientDecodeString

instance Show Hostname where
  showsPrec _ (Hostname h) =
    showChar '"' . showHostBytes h . showChar '"'

-- Fortunately, we do not have to worry about quotes or
-- backslashes showing up in here.
showHostBytes :: Bytes -> ShowS
showHostBytes (Bytes arr off len) s = if len > 0
  then unsafeChr (PM.indexByteArray arr off)
     : showHostBytes (Bytes arr (off + 1) (len - 1)) s
  else s

unsafeChr :: Word8 -> Char
unsafeChr w =
  let !(Exts.I# i) = fromIntegral w
   in Exts.C# (Exts.chr# i)

-- | The root domain, commonly written out as a period. 
root :: Hostname
root = Hostname $ Bytes
  ( runST $ do
      marr <- PM.newByteArray 1
      PM.writeByteArray marr 0 (c2w '.')
      PM.unsafeFreezeByteArray marr
  ) 0 1

-- | Decode a hostname from a 'String' leniently. This is used by
-- the 'IsString' instance. Roughly, the rules are:
--
-- * Leading and trailing hyphens in labels are omitted.
-- * Repeated periods are replaced by a single period.
-- * The empty string is the root domain.
lenientDecodeString :: String -> Hostname
lenientDecodeString = \case
  [] -> root
  cs -> runST $ do
    marr <- PM.newByteArray 255
    lenientStringOuter marr 0 cs
    -- The byte array is shrunk by the helper function.
    len <- PM.getSizeofMutableByteArray marr
    downcaseInPlace (MutableBytes marr 0 len)
    arr <- PM.unsafeFreezeByteArray marr
    pure (Hostname (Bytes arr 0 len))

isAlphaNumAscii :: Char -> Bool
isAlphaNumAscii c = 
     (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')

-- This is called when the index is at the character immediately
-- following a period or when we are at the beginning of the string.
-- For example, this will be called at all of these positions:
--   my-.example..-.foo-bar.gov.
--   ^   ^       ^^^^       ^   ^
-- Although the input is malformed, this will strip out the
-- characters that show up in bad places.
lenientStringOuter :: MutableByteArray s -> Int -> [Char] -> ST s ()
lenientStringOuter !marr !ix cs0 = case cs0 of
  [] -> if ix > 0
    then shrinkMutableByteArray marr ix
    else do
      PM.writeByteArray marr 0 (c2w '.')
      shrinkMutableByteArray marr 1
  c : cs -> if isAlphaNumAscii c
    then lenientStringInner marr ix 0 (c : cs)
    else lenientStringOuter marr ix cs

lenientStringInner :: MutableByteArray s -> Int -> Int -> [Char] -> ST s ()
lenientStringInner !marr !ix !lblLen cs0 = case cs0 of
  [] -> do
    -- Remove trailing hyphens by shrinking the byte array.
    ix' <- trimTrailingHyphens marr (ix - 1)
    shrinkMutableByteArray marr ix'
  c : cs -> if isAlphaNumAscii c || c == '-' || c == '.'
    then if ix < 255
      then if c /= '.'
        then if lblLen < 63
          then do
            PM.writeByteArray marr ix (c2w c)
            lenientStringInner marr (ix + 1) (lblLen + 1) cs
          else do
            -- If the label is too long, skip until the next period.
            let go ds0 = case ds0 of
                  [] -> do
                    ix' <- trimTrailingHyphens marr (ix - 1)
                    shrinkMutableByteArray marr ix'
                  d : ds -> case d of
                    '.' -> do
                      ix' <- trimTrailingHyphens marr (ix - 1)
                      PM.writeByteArray marr ix' (c2w '.')
                      lenientStringOuter marr (ix' + 1) ds
                    _ -> go ds
            go cs
        else do
          -- Check for trailing hyphens to eliminate. There is no
          -- need to check if the index goes below zero since that
          -- cannot happen.
          let go !z = do
                w <- PM.readByteArray marr z
                if w == c2w '-'
                  then go (z - 1)
                  else pure (z + 1)
          ix' <- go (ix - 1)
          PM.writeByteArray marr ix' (c2w '.')
          lenientStringOuter marr (ix' + 1) cs
      else do
        let go !z = do
              w <- PM.readByteArray marr z
              if w == c2w '.'
                then shrinkMutableByteArray marr z
                else go (z - 1)
        go 254
    else lenientStringInner marr ix lblLen cs

-- | Decode a hostname by interpreting the byte sequence as
-- UTF-8 encoded text. If the byte sequence does not have any
-- uppercase characters, this reuses the byte array, avoiding
-- any allocations.
--
-- This decodes either a single period or an empty byte
-- array as the root domain.
--
-- There is currently no support for non-English characters.
-- This would require conversion to Punycode. While this is
-- certainly within the scope of this library, it is difficult
-- to implement and has been omitted due to lack of necessity.
decodeUtf8 :: Bytes -> Maybe Hostname
decodeUtf8 (Bytes arr off len) = if len > 1 && len < 255
  then go off (off + len) 1 0 0 0
  else if len == 1
    then if PM.indexByteArray arr off == c2w '.'
      then Just root
      else go off (off + len) 1 0 0 0
    else if len == 0
      then Just root
      -- reject when len >= 255
      else Nothing
  where
  -- We check for two things as we fold over the byte
  -- sequence: (1) is this a valid hostname and (2) is it
  -- already in canonical form (no uppercase)?
  go :: Int -> Int -> Int -> Int -> Int -> Int -> Maybe Hostname
  go !ix !end !cannon !canDot !canHyphen !lblLen = if ix < end
    then
      let b = PM.indexByteArray arr ix :: Word8
       in if | b >= c2w 'a' && b <= c2w 'z' -> if lblLen < 63
                 then go (ix + 1) end cannon 1 1 (lblLen + 1)
                 else Nothing
             | b >= c2w 'A' && b <= c2w 'Z' -> if lblLen < 63
                 then go (ix + 1) end 0 1 1 (lblLen + 1)
                 else Nothing
             | b >= c2w '0' && b <= c2w '9' -> if lblLen < 63
                 then go (ix + 1) end cannon 1 1 (lblLen + 1)
                 else Nothing
             | b == c2w '-' -> if lblLen < 63
                 then case canHyphen of
                   1 -> go (ix + 1) end cannon 0 1 (lblLen + 1)
                   _ -> Nothing
                 else Nothing
             | b == c2w '.' -> case canDot of
                 1 -> go (ix + 1) end cannon 0 0 0
                 _ -> Nothing
             | otherwise -> Nothing
    -- It is safe to look at the last character by subtracting
    -- one since since we checked earlier to ensure that the
    -- length is at least one.
    else if PM.indexByteArray arr (ix - 1) /= c2w '-'
      then if cannon == 1
        then Just (Hostname (Bytes arr off len))
        else Just (Hostname (downcase (Bytes arr off len)))
      else Nothing
        
downcase :: Bytes -> Bytes
downcase (Bytes arr off len) =
  let res = runST $ do
        marr <- PM.newByteArray len
        let go !ixDst !ixSrc = if ixDst >= 0
              then do
                let wSrc = PM.indexByteArray arr ixSrc :: Word8
                    wDst = if wSrc >= c2w 'A' && wSrc <= c2w 'Z'
                      then wSrc + 32
                      else wSrc
                PM.writeByteArray marr ixDst wDst
                go (ixDst - 1) (ixSrc - 1)
              else pure ()
        go (len - 1) (off + len - 1)
        PM.unsafeFreezeByteArray marr
   in Bytes res 0 len

downcaseInPlace :: MutableBytes s -> ST s ()
downcaseInPlace (MutableBytes arr off len) = if len > 0
  then do
    w <- PM.readByteArray arr off
    let w' = if w >= c2w 'A' && w <= c2w 'Z' then w + 32 else w
    PM.writeByteArray arr off w'
    downcaseInPlace (MutableBytes arr (off + 1) (len - 1))
  else pure ()

-- This does not check to see if the index goes below zero.
trimTrailingHyphens :: MutableByteArray s -> Int -> ST s Int
trimTrailingHyphens !arr !ix = do
  w <- PM.readByteArray arr ix
  if w /= c2w '-'
    then pure (ix + 1)
    else trimTrailingHyphens arr (ix - 1)

c2w :: Char -> Word8
c2w = fromIntegral . ord

shrinkMutableByteArray :: forall s. MutableByteArray s -> Int -> ST s ()
shrinkMutableByteArray (MutableByteArray x) i =
  PM.shrinkMutablePrimArray (MutablePrimArray x :: MutablePrimArray s Word8) i
