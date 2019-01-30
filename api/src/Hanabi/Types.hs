{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Hanabi.Types where

import GHC.Generics

import           Control.Lens
import           Data.Time.Clock     (UTCTime)
import Data.Aeson

data Game = Game
  { _gameVersion :: Integer
  , _gameModified :: UTCTime
  } deriving (Generic)

makeLenses ''Game

mkGame :: UTCTime -> Game
mkGame now = Game
  { _gameVersion = 1
  , _gameModified = now
  }

instance ToJSON Game
