module Domain
  ( Domain(..)
  ) where

newtype Domain = Domain Bytes

fromBytes :: Bytes -> Maybe Domain

