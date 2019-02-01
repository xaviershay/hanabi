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

data Color = Red | Blue | Green | Yellow | White deriving (Show, Enum, Generic)
data Location = Hand PlayerId | Deck | Board | Discard deriving (Show, Generic)

data Visibility = Hidden | HiddenFrom PlayerId | Visible deriving (Show, Generic)

data Card = Card
  { _cardId :: CardId
  , _cardRank :: Int
  , _cardColor :: Color
  , _cardLocation :: Location
  , _cardVisibility :: Visibility
  } deriving (Show, Generic)

data PlayerChoice =
  ChoicePlayCard CardId
  deriving (Show)

data Choice = Choice PlayerId PlayerChoice deriving (Show)

data Game = Game
  { _gameVersion :: Integer
  , _gameModified :: UTCTime
  , _gameCards :: M.HashMap CardId Card
  } deriving (Generic)

makeLenses ''Game
makeLenses ''Card

mkGame :: UTCTime -> Game
mkGame now = Game
  { _gameVersion = 1
  , _gameModified = now
  , _gameCards = M.fromList [(CardId 1, mkFakeCard)] --mempty
  }

mkFakeCard = Card
  { _cardId = CardId 1
  , _cardRank = 2
  , _cardColor = Red
  , _cardLocation = Hand (PlayerId 123)
  , _cardVisibility = Visible
  }

apply :: Choice -> Game -> Game
apply (Choice pid (ChoicePlayCard cid)) =
  over
    (gameCards . at cid . _Just)
    (\card ->
        case view cardLocation card of
          Hand cardPlayerId -> if cardPlayerId == pid then
                                 set cardLocation Board card
                               else
                                 card
          _ -> card
    )

apply _ = error "unimplemented"


instance ToJSON CardId
instance ToJSONKey CardId
instance ToJSON Visibility
instance ToJSON Location
instance ToJSON PlayerId
instance ToJSON Color
instance ToJSON Card
instance ToJSON Game
instance Hashable CardId
