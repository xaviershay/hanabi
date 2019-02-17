{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Hanabi.Types where

import           Hanabi.Extras.Aeson
import           Hanabi.Prelude

import           Control.Lens              (at, makeLenses, view, _Just)
import           Control.Monad             (forM_, unless, when)
import           Control.Monad.Freer       (Eff, Members, run, runM)
import           Control.Monad.Freer.Error (Error, runError, throwError)
import           Control.Monad.Freer.State (State (..), get, gets, put,
                                            runState)
import           Data.Aeson
import           Data.Hashable
import qualified Data.HashMap.Strict       as M
import qualified Data.List
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import           Data.Time.Clock           (UTCTime)
import           GHC.Generics

newtype PlayerId = PlayerId String
  deriving stock (Show, Eq, Generic)
  deriving ToJSON via String
  deriving FromJSON via String

newtype CardId = CardId Int
  deriving stock (Show, Eq, Generic)
  deriving ToJSON via Int
  deriving FromJSON via Int
  deriving ToJSONKey via Int
  deriving Hashable via Int

type Rank = Int

data Color = Red | Blue | Green | Yellow | White
  deriving stock (Show, Enum, Generic, Bounded, Ord, Eq)
  deriving ToJSON via LowercaseShow Color
  deriving FromJSON via LowercaseShow Color

data Location = Hand PlayerId | Deck | Table | Discard deriving (Show, Generic, Eq)

instance ToJSON Location where
  toJSON = toJSON . toList
    where
      toList (Hand (PlayerId pid)) = ["hand", pid]
      toList Deck = ["deck"]
      toList Discard = ["discard"]
      toList Table = ["table"]

allColors = [minBound :: Color .. ]
allRanks = [1..5]

data Card = Card
  { _cardId :: CardId
  , _cardRank :: Rank
  , _cardColor :: Color
  , _cardLocation :: Location
  , _cardPossibleRanks :: S.Set Rank
  , _cardPossibleColors :: S.Set Color
  }
  deriving stock (Show, Generic)
  deriving ToJSON via StripPrefix "_card" Card

data PlayerChoice =
  ChoicePlayCard CardId
  | ChoiceHintRank PlayerId Rank
  | ChoiceHintColor PlayerId Color
  | ChoiceDiscardCard CardId
  deriving (Show)

data Choice = Choice PlayerId PlayerChoice deriving (Show)

instance FromJSON PlayerChoice where
  parseJSON = withObject "PlayerChoice" $ \v -> do
    action :: T.Text <- v .: "type"
    case action of
      "play"      -> ChoicePlayCard    <$> v .: "id"
      "discard"   -> ChoiceDiscardCard <$> v .: "id"
      "hintRank"  -> ChoiceHintRank    <$> v .: "player" <*> v .: "rank"
      "hintColor" -> ChoiceHintColor   <$> v .: "player" <*> v .: "color"
      _           -> fail "unknown choice"

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
  deriving stock (Generic)

data RedactedCard = RedactedCard
  { _redactedId :: CardId
  , _redactedLocation :: Location
  , _redactedColor :: Maybe Color
  , _redactedRank :: Maybe Rank
  , _redactedPossibleRanks :: S.Set Rank
  , _redactedPossibleColors :: S.Set Color
  }
  deriving stock (Generic)

instance ToJSON RedactedCard where
  toJSON card =
    let
      (Object base) = toJSON
        (StripPrefix card :: StripPrefix "_redacted" RedactedCard)
      -- Just to cut down on noise, remove possible* keys when card is not in
      -- hand.
      f = case _redactedLocation card of
            Hand _ -> id
            _      -> M.delete "possibleRanks" . M.delete "possibleColors"

    in

    toJSON $ f base

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
  { _redactedId = view cardId base
  , _redactedLocation = view cardLocation base
  , _redactedRank = rank
  , _redactedColor = color
  , _redactedPossibleRanks = view cardPossibleRanks base
  , _redactedPossibleColors = view cardPossibleColors base
  }

data RedactedGame = RedactedGame PlayerId Game

instance ToJSON RedactedGame where
  toJSON (RedactedGame pid game) =
    let
      (Object base) = toJSON (StripPrefix game :: StripPrefix "_game" Game)
      (Object redacted) = object
          [ "cards" .= toJSON (redact pid . M.elems . view gameCards $ game)
          ]
    in

    toJSON $ redacted <> base

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
