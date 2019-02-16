{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
module Hanabi.Prelude
  ( module Control.Lens
  , module Debug.Trace
  , headMaybe
  , note
  , whenJust
  , assign
  , modifying
  )
  where

import Control.Monad.Freer (Eff, Members, run, runM)
import Control.Monad.Freer.Error (Error, throwError, runError)
import Control.Monad.Freer.State (State(..), get, gets, put, runState)
import Control.Lens (ASetter, view, over, set)
import Debug.Trace

headMaybe :: [a] -> Maybe a
headMaybe xs =
  case xs of
    (x:_) -> Just x
    []    -> Nothing

note :: Members '[ Error e ] effs => e -> Maybe a -> Eff effs a
note err Nothing = throwError err
note err (Just x) = return x

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing f = return ()
whenJust (Just x) f = f x

-- =========
-- Reimplementations of Control.Lens MTL helpers to work with freer-simple
assign :: Members '[ State s ] effs => ASetter s s a b -> b -> Eff effs ()
assign accessor value = get >>= put . set accessor value

modifying :: Members '[ State s ] effs => ASetter s s a b -> (a -> b) -> Eff effs ()
modifying accessor f = get >>= put . over accessor f
-- =========
