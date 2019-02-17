{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}

module Hanabi.Types where

import Hanabi.Prelude
import Hanabi.Extras.Aeson

import Control.Monad.Freer (Eff, Members, run, runM)
import Control.Monad.Freer.Error (Error, throwError, runError)
import Control.Monad.Freer.State (State(..), get, gets, put, runState)
import Control.Lens (view, makeLenses, _Just, at)
import Control.Monad (unless, when, forM_)
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.Aeson
import qualified Data.List
import qualified Data.Set as S
import Data.Time.Clock (UTCTime)
import GHC.Generics

data RedactedGame = RedactedGame PlayerId Game

newtype PlayerId = PlayerId String
  deriving stock (Show, Eq, Generic)
  deriving FromJSON via String

newtype CardId = CardId Int
  deriving stock (Show, Eq, Generic)
  deriving ToJSON via Int
  deriving FromJSON via Int
  deriving Hashable via Int

type Rank = Int

data Color = Red | Blue | Green | Yellow | White deriving (Show, Enum, Generic, Bounded, Ord, Eq)
data Location = Hand PlayerId | Deck | Table | Discard deriving (Show, Generic, Eq)
allColors = [minBound :: Color .. ]
allRanks = [1..5]

data Card = Card
  { _cardId :: CardId
  , _cardRank :: Rank
  , _cardColor :: Color
  , _cardLocation :: Location
  , _cardPossibleRanks :: S.Set Rank
  , _cardPossibleColors :: S.Set Color
  } deriving (Show, Generic)

data PlayerChoice =
  ChoicePlayCard CardId
  | ChoiceHintRank PlayerId Rank
  | ChoiceHintColor PlayerId Color
  | ChoiceDiscardCard CardId
  deriving (Show)

data Choice = Choice PlayerId PlayerChoice deriving (Show)

type CardMap = M.HashMap CardId Card

data GameSpec = GameSpec
  { _gameSpecPlayers :: [PlayerId]
  }
  deriving stock (Show, Generic)
  deriving FromJSON via StripPrefix "_gameSpec" GameSpec


data Game = Game
  { _gameVersion :: Integer
  , _gameModified :: UTCTime
  , _gameCards :: CardMap
  , _gameHints :: Integer
  , _gameMaxHints :: Integer
  , _gameExplosions :: Integer
  , _gameCurrentPlayer :: PlayerId
  }

data RedactedCard = RedactedCard
  { _redactedCardId :: CardId
  , _redactedLocation :: Location
  , _redactedColor :: Maybe Color
  , _redactedRank :: Maybe Rank
  , _redactedPossibleRanks :: S.Set Rank
  , _redactedPossibleColors :: S.Set Color
  }

makeLenses ''Game
makeLenses ''Card
makeLenses ''RedactedCard

mkGame :: UTCTime -> Game
mkGame now = Game
  { _gameVersion = 1
  , _gameHints = 2
  , _gameMaxHints = 3
  , _gameExplosions = 3
  , _gameModified = now
  , _gameCurrentPlayer = PlayerId "Xavier"
  , _gameCards = M.fromList
    [ (CardId 1, set cardId (CardId 1) $ set cardLocation Discard mkFakeCard)
    , (CardId 2, set cardId (CardId 2) $ set cardLocation (Hand (PlayerId "Xavier")) mkFakeCard)
  --  , (CardId 3, set cardId (CardId 3) $ set cardLocation (Hand (PlayerId "Xavier")) mkFakeCard)
  --  , (CardId 4, set cardColor Yellow $ set cardId (CardId 4) $ set cardLocation (Hand (PlayerId "Xavier")) mkFakeCard)
  --  , (CardId 5, set cardRank 3 $ set cardId (CardId 5) $ set cardLocation (Hand (PlayerId "Xavier")) mkFakeCard)
    ] --mempty
  }

mkFakeCard = Card
  { _cardId = CardId 1
  , _cardLocation = Discard
  , _cardRank = 2
  , _cardColor = Red
  , _cardPossibleRanks = S.fromList allRanks
  , _cardPossibleColors = S.fromList allColors
  }

mkRedactedCard :: Card -> Maybe Rank -> Maybe Color -> RedactedCard
mkRedactedCard base rank color = RedactedCard
  { _redactedCardId = view cardId base
  , _redactedLocation = view cardLocation base
  , _redactedRank = rank
  , _redactedColor = color
  , _redactedPossibleRanks = view cardPossibleRanks base
  , _redactedPossibleColors = view cardPossibleColors base
  }

redact :: PlayerId -> [Card] -> [RedactedCard]
redact pid = map (redactCard pid)
  where
    redactCard pid card =
      case view cardLocation card of
        Deck                          -> mkRedactedCard card Nothing Nothing
        Hand cardPid | cardPid == pid -> mkRedactedCard card Nothing Nothing
        _ -> mkRedactedCard card
               (Just $ view cardRank card)
               (Just $ view cardColor card)
