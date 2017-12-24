module Kutyus
    ( Frame
    ) where

import qualified Data.ByteString.Lazy as B

data Message a = Message

data Frame a = Frame
  { version :: Int
  , message :: Message a
  , signature :: B.ByteString
  }
