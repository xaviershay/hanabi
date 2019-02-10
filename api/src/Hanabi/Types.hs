{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Hanabi.Types where

import GHC.Generics

import Control.Lens hiding (Choice)
import Data.Aeson
import qualified Data.List
import Data.Time.Clock (UTCTime)
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.Hashable

data RedactedGame = RedactedGame PlayerId Game

newtype PlayerId = PlayerId String deriving (Show, Eq, Generic)
newtype CardId = CardId Int deriving (Show, Eq, Generic)
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
  deriving (Show)

data Choice = Choice PlayerId PlayerChoice deriving (Show)

type CardMap = M.HashMap CardId Card

data Game = Game
  { _gameVersion :: Integer
  , _gameModified :: UTCTime
  , _gameCards :: CardMap
  , _gameHints :: Integer
  , _gameExplosions :: Integer
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
  , _gameHints = 5
  , _gameExplosions = 3
  , _gameModified = now
  , _gameCards = M.fromList
    [ (CardId 1, set cardId (CardId 1) $ mkFakeCard)
    , (CardId 2, set cardId (CardId 2) $ set cardLocation (Hand (PlayerId "Xavier")) mkFakeCard)
  --  , (CardId 3, set cardId (CardId 3) $ set cardLocation (Hand (PlayerId "Xavier")) mkFakeCard)
  --  , (CardId 4, set cardColor Yellow $ set cardId (CardId 4) $ set cardLocation (Hand (PlayerId "Xavier")) mkFakeCard)
  --  , (CardId 5, set cardRank 3 $ set cardId (CardId 5) $ set cardLocation (Hand (PlayerId "Xavier")) mkFakeCard)
    ] --mempty
  }

mkFakeCard = Card
  { _cardId = CardId 1
  , _cardLocation = Deck
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

apply :: Choice -> Game -> Game
apply (Choice pid (ChoicePlayCard cid)) state =
  let maybeCard = view (gameCards . at cid) state in

  case maybeCard of
    Just chosenCard ->
      let maxRankOnTable =
            maximum
            . (:) 0
            . map (view cardRank)
            . filter
              (\card ->
                (view cardLocation card == Table)
                && view cardColor chosenCard == view cardColor card)
            . M.elems
            . view gameCards
            $ state
            in

      case view cardLocation chosenCard of
        Hand cardPlayerId ->
          if cardPlayerId == pid && view cardRank chosenCard == maxRankOnTable + 1 then
            set (gameCards . at cid . _Just . cardLocation) Table state
          else
            set (gameCards . at cid . _Just . cardLocation) Discard
              . over gameExplosions (\x -> x - 1)
              $ state
        _ -> state
    Nothing -> state

apply (Choice pid (ChoiceHintRank targetPid rank)) state
  | pid /= targetPid && view gameHints state > 0 =
    let handCards =
          filter (\card -> view cardLocation card == Hand targetPid)
          . M.elems
          $ view gameCards state in

    let (matchedCs, unmatchedCs) = Data.List.partition ((==) rank . view cardRank) handCards in

    if length matchedCs > 0 then
      -- TODO: Consider replacing foldr with more composable function
      -- TODO: Figure out which strict fold to use
      let cardAccessor = \card -> gameCards . at (view cardId card) . _Just . cardPossibleRanks in
      let state1 = foldr
            (\card -> set (cardAccessor card) (S.singleton rank))
            state
            matchedCs in

      let state2 = foldr
            (\card -> over (cardAccessor card) (S.delete rank))
            state1
            unmatchedCs in

      over gameHints (\x -> x - 1) state2
    else
      state

apply (Choice pid (ChoiceHintColor targetPid color)) state
  | pid /= targetPid && view gameHints state > 0 =
    let handCards =
          filter (\card -> view cardLocation card == Hand targetPid)
          . M.elems
          $ view gameCards state in

    let (matchedCs, unmatchedCs) = Data.List.partition ((==) color . view cardColor) handCards in

    if length matchedCs > 0 then
      -- TODO: Consider replacing foldr with more composable function
      -- TODO: Figure out which strict fold to use
      let cardAccessor = \card -> gameCards . at (view cardId card) . _Just . cardPossibleColors in
      let state1 = foldr
            (\card -> set (cardAccessor card) (S.singleton color))
            state
            matchedCs in

      let state2 = foldr
            (\card -> over (cardAccessor card) (S.delete color))
            state1
            unmatchedCs in

      over gameHints (\x -> x - 1) state2
    else
      state

apply _ _ = error "unimplemented"


instance Hashable CardId
