{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Hanabi.Types where

import Hanabi.Prelude

import Control.Lens hiding (Choice)
import Control.Monad (unless, when)
import Data.Aeson
import qualified Data.HashMap.Strict as M
import Data.Hashable
import qualified Data.List
import qualified Data.Set as S
import Data.Time.Clock (UTCTime)
import GHC.Generics

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
  | ChoiceDiscardCard CardId
  deriving (Show)

data Choice = Choice PlayerId PlayerChoice deriving (Show)

type CardMap = M.HashMap CardId Card

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


applyWithInvalid :: Choice -> Game -> Either String Game
applyWithInvalid (Choice pid playerChoice) state = do
  unless (pid == view gameCurrentPlayer state) $
    Left $ "cannot make a choice when not your turn"
  
  case playerChoice of
    ChoicePlayCard cid           -> applyPlay cid state
    ChoiceDiscardCard cid        -> applyDiscard cid state
    ChoiceHintRank target rank   -> applyHintRank target rank state
    ChoiceHintColor target color -> applyHintColor target color state

requireCard :: CardId -> Game -> Either String Card
requireCard cid = note "Card does not exist" . view (gameCards . at cid)

applyPlay cid state = do
  let maybeCard = view (gameCards . at cid) state

  chosenCard <- requireCard cid state

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

  -- TODO: Possible refactoring?
  validateInHand chosenCard state

  Right $
    if view cardRank chosenCard == maxRankOnTable + 1 then
      set (gameCards . at cid . _Just . cardLocation) Table state
    else
      set (gameCards . at cid . _Just . cardLocation) Discard
        . over gameExplosions (\x -> x - 1)
        $ state

  where
    validateInHand :: Card -> Game -> Either String Card
    validateInHand card state =
      let currentPlayerId = view gameCurrentPlayer state in
      case view cardLocation card of
        Hand cardPlayerId | cardPlayerId == currentPlayerId -> Right card
        _ -> Left "card is not in hand"


applyDiscard cid state = do
  unless (view gameHints state < view gameMaxHints state) $
    Left "cannot discard when at max hints"

  chosenCard <- requireCard cid state

  let currentPlayerId = view gameCurrentPlayer state

  let maybeTopCard = 
        headMaybe
          . filter (\card -> (view cardLocation card == Deck))
          . M.elems
          . view gameCards
        $ state

  -- Only draw if there are cards left in the deck
  let drawF =
        maybe id
          (\topCard ->
            set
              (gameCards . at (view cardId topCard) . _Just . cardLocation)
              (Hand currentPlayerId))
          maybeTopCard

  Right $ over gameHints (\x -> x + 1)
    . set (gameCards . at cid . _Just . cardLocation) Discard
    . drawF
    $ state

applyHintRank  = applyHint cardRank cardPossibleRanks
applyHintColor = applyHint cardColor cardPossibleColors

applyHint accessor possibleAccessor targetPid hint state = do
  let currentPlayerId = view gameCurrentPlayer state

  when (currentPlayerId == targetPid) $
    Left "cannot hint self"

  unless (view gameHints state > 0) $
    Left "insufficient hints remaining"

  let handCards =
        filter (\card -> view cardLocation card == Hand targetPid)
        . M.elems
        $ view gameCards state

  let (matchedCs, unmatchedCs) = Data.List.partition ((==) hint . view accessor) handCards

  unless (length matchedCs > 0) $
    Left "can only hint ranks in hand"

  -- TODO: Consider replacing foldr with more composable function
  -- TODO: Figure out which strict fold to use
  let cardAccessor = \card -> gameCards . at (view cardId card) . _Just . possibleAccessor
  let state1 = foldr
        (\card -> set (cardAccessor card) (S.singleton hint))
        state
        matchedCs

  let state2 = foldr
        (\card -> over (cardAccessor card) (S.delete hint))
        state1
        unmatchedCs

  Right $ over gameHints (\x -> x - 1) state2

instance Hashable CardId
