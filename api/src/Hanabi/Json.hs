{-# LANGUAGE OverloadedStrings #-}

module Hanabi.Json where

import Hanabi.Types
import Hanabi.Prelude

import Data.Aeson
import qualified Data.HashMap.Strict as M

instance ToJSON CardId
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
  toJSON card = object
    [ "id"       .= view redactedCardId card
    , "location" .= view redactedLocation card
    , "color"    .= view redactedColor card
    , "rank"     .= view redactedRank card
    ]

instance ToJSON RedactedGame where
  toJSON (RedactedGame pid game) = object
    [ "version"      .= view gameVersion game
    , "lastModified" .= view gameModified game
    , "cards"        .= (redact pid . M.elems . view gameCards $ game)
    ]
