module Kutyus.Feed
  ( Feed
  ) where

import Kutyus.Message

data Feed = Feed
  { author :: AuthorId
  } deriving (Show, Eq)

