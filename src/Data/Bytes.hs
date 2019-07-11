module Data.Bytes
  ( Bytes(..)
  ) where

import Data.Primitive (ByteArray)

data Bytes = Bytes
  !ByteArray -- payload
  !Int -- offset
  !Int -- length
