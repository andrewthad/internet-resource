module Internet.Fqdn
  ( Fqdn(..)
  ) where

newtype Fqdn = Fqdn Hostname

fromBytes :: Bytes -> Maybe Fqdn

-- | If the hostname ends in a top-level domain, it is
--   also a FQDN.
fromHostname :: Hostname -> Fqdn
fromHostname (Hostname 


