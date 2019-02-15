module Hanabi.Prelude
  ( module Control.Lens
  , module Debug.Trace
  , headMaybe
  , note
  )
  where

import Control.Lens (view, over, set)
import Debug.Trace

headMaybe :: [a] -> Maybe a
headMaybe xs =
  case xs of
    (x:_) -> Just x
    []    -> Nothing

note :: b -> Maybe a -> Either b a
note err Nothing = Left err
note err (Just x) = Right x
