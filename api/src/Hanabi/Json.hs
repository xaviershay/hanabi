{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Hanabi.Json where

import Hanabi.Types
import Hanabi.Prelude

import Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

instance ToJSON Color where
  toJSON Red    = "red"
  toJSON Green  = "green"
  toJSON White  = "white"
  toJSON Blue   = "blue"
  toJSON Yellow = "yellow"

instance ToJSON Location where
  toJSON = toJSON . toList
    where
      toList (Hand (PlayerId pid)) = ["hand", pid]
      toList Deck = ["deck"]
      toList Discard = ["discard"]
      toList Table = ["table"]

instance ToJSON RedactedCard where
  toJSON card = object $
    [ "id"       .= view redactedCardId card
    , "location" .= view redactedLocation card
    , "color"    .= view redactedColor card
    , "rank"     .= view redactedRank card
    ] <> case view redactedLocation card of
           Hand _ ->
             [ "possibleRanks"  .= view redactedPossibleRanks card
             , "possibleColors" .= view redactedPossibleColors card
             ]
           _ -> mempty

instance ToJSON RedactedGame where
  toJSON (RedactedGame pid game) = object
    [ "version"      .= view gameVersion game
    , "hints"        .= view gameHints game
    , "explosions"   .= view gameExplosions game
    , "lastModified" .= view gameModified game
    , "cards"        .= (redact pid . M.elems . view gameCards $ game)
    ]

instance FromJSON Color where
  parseJSON = withText "Color" $
    \case
      "red"   -> pure Red
      "green" -> pure Green
      _ -> fail "unknown color"

instance FromJSON PlayerChoice where
  parseJSON = withObject "PlayerChoice" $ \v -> do
    action :: T.Text <- v .: "type"
    case action of
      "play"      -> ChoicePlayCard    <$> v .: "id"
      "discard"   -> ChoiceDiscardCard <$> v .: "id"
      "hintRank"  -> ChoiceHintRank    <$> v .: "player" <*> v .: "rank"
      "hintColor" -> ChoiceHintColor   <$> v .: "player" <*> v .: "color"
      _           -> fail "unknown choice"
