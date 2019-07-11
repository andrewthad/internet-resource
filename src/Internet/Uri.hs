module Internet.Uri
  ( Uri(..)
  , Scheme(..)
  ) where

data Uri = Uri
  {-# UNPACK #-} !Bytes -- Encoded URI
  {-# UNPACK #-} !Word64 -- Four Word16s: user, password, host, port
  {-# UNPACK #-} !Word64 -- Four Word16s: path, query, fragment, zero
  {-# UNPACK #-} !IP
  -- IPv4 or IPv6 address, uses zero bits address ::/128 to communicate
  -- that the address is a registered name instead of an address.

scheme :: Uri -> Scheme
scheme (Uri enc w1 _ _) = enc



