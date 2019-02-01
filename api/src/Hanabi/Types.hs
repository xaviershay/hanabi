{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Hanabi.Types where

import GHC.Generics

import Control.Lens hiding (Choice)
import Data.Aeson
import Data.Time.Clock (UTCTime)
import qualified Data.HashMap.Strict as M
import Data.Hashable

newtype PlayerId = PlayerId Int deriving (Show, Eq, Generic)
newtype CardId = CardId Int deriving (Show, Eq, Generic)
type Rank = Int

data Color = Red | Blue | Green | Yellow | White deriving (Show, Enum, Generic)
data Location = Hand PlayerId | Deck | Board | Discard deriving (Show, Generic)

data Card = Card
  { _cardId :: CardId
  , _cardRank :: Rank
  , _cardColor :: Color
  , _cardLocation :: Location
  } deriving (Show, Generic)

data PlayerChoice =
  ChoicePlayCard CardId
  deriving (Show)

data Choice = Choice PlayerId PlayerChoice deriving (Show)

type CardMap = M.HashMap CardId Card

data Game = Game
  { _gameVersion :: Integer
  , _gameModified :: UTCTime
  , _gameCards :: CardMap
  } deriving (Generic)

makeLenses ''Game
makeLenses ''Card

mkGame :: UTCTime -> Game
mkGame now = Game
  { _gameVersion = 1
  , _gameModified = now
  , _gameCards = M.fromList [(CardId 1, mkFakeCard)] --mempty
  }

data RedactedCard = RedactedCard
  { _redactedCardId :: CardId
  , _redactedLocation :: Location
  , _redactedColor :: Maybe Color
  , _redactedRank :: Maybe Rank
  }

mkFakeCard = Card
  { _cardId = CardId 1
  , _cardLocation = Hand (PlayerId 123)
  , _cardRank = 2
  , _cardColor = Red
  }

mkRedactedCard :: Card -> Maybe Rank -> Maybe Color -> RedactedCard
mkRedactedCard base rank color = RedactedCard
  { _redactedCardId = view cardId base
  , _redactedLocation = view cardLocation base
  , _redactedRank = rank
  , _redactedColor = color
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

apply :: Choice -> Game -> Game
apply (Choice pid (ChoicePlayCard cid)) =
  over
    (gameCards . at cid . _Just)
    (\card ->
        case view cardLocation card of
          Hand cardPlayerId -> if cardPlayerId == pid then
                                 set cardLocation Board
                                 $ card
                               else
                                 card
          _ -> card
    )

apply _ = error "unimplemented"

instance ToJSON CardId
instance ToJSONKey CardId
instance ToJSON Location
instance ToJSON PlayerId
instance ToJSON Color
instance ToJSON Card
instance ToJSON Game
instance Hashable CardId
