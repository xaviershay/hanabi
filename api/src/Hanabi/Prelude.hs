module Hanabi.Prelude
  ( module Control.Lens
  , module Debug.Trace
  , headMaybe
  )
  where

import Control.Lens (view, over, set)
import Debug.Trace

headMaybe :: [a] -> Maybe a
headMaybe xs =
  case xs of
    (x:_) -> Just x
    []    -> Nothing
