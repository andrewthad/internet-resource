module Internet.Host
  ( Hostname(..)
  ) where

-- | An Internet hostname as described by
-- <https://tools.ietf.org/html/rfc1123 RFC-1123>.
-- Such a hostname is used by DNS, but it need not
-- be fully qualified. The allowed characters ranges are
-- @a-z@ (case insensitive),@0-9@,and hyphen. To support efficient equality
-- testing and comparison, all hostnames are canonized to lower case.
newtype Hostname = Hostname
  {-# UNPACK #-} !Bytes -- Encoded host name
  {-# UNPACK #-} !(PrimArray Word8) -- Array of Word8s, subdomain dot locations

-- | Parse a hostname from some bytes. If the hostname is already
-- lowercase, this does not copy the bytes.
fromBytes :: Bytes -> Hostname
fromBytes (Bytes arr off len) = 


