module Hanabi.Prelude
  ( module Control.Lens
  , module Debug.Trace
  , headMaybe
  , note
  , whenJust
  )
  where

import Control.Lens (view, over, set)
import Control.Monad.Except
import Debug.Trace

headMaybe :: [a] -> Maybe a
headMaybe xs =
  case xs of
    (x:_) -> Just x
    []    -> Nothing

note :: MonadError b m => b -> Maybe a -> m a
note err Nothing = throwError err
note err (Just x) = return x

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing f = return ()
whenJust (Just x) f = f x

