{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hanabi.Apply
  ( apply
  ) where

import Hanabi.Prelude
import Hanabi.Types

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

type AppEff effs = Members '[ Error String, State Game ] effs

runApp :: Game -> Eff '[ Error String, State Game ] a -> Either String Game
runApp state m =
  let (val, newGame) = run . runState state . runError $ m in

  val >>= \_ -> Right newGame

apply :: Choice -> Game -> Either String Game
apply (Choice pid playerChoice) state = do
  unless (pid == view gameCurrentPlayer state) $
    Left $ "cannot make a choice when not your turn"

  runApp state $ case playerChoice of
    ChoicePlayCard cid           -> applyPlay cid
    ChoiceDiscardCard cid        -> applyDiscard cid
    ChoiceHintRank target rank   -> applyHintRank target rank
    ChoiceHintColor target color -> applyHintColor target color

requireCard :: AppEff effs => CardId -> Eff effs Card
requireCard cid =
  get >>= note "Card does not exist" . view (gameCards . at cid)

applyPlay :: AppEff effs => CardId -> Eff effs ()
applyPlay cid = do
  chosenCard <- requireCard cid

  maxRankOnTable <-
    gets $ maximum
    . (:) 0
    . map (view cardRank)
    . filter
      (\card ->
        (view cardLocation card == Table)
        && view cardColor chosenCard == view cardColor card)
    . M.elems
    . view gameCards

  validateInHand chosenCard

  if view cardRank chosenCard == maxRankOnTable + 1 then
    assign (gameCards . at cid . _Just . cardLocation) Table
  else
    do
      assign (gameCards . at cid . _Just . cardLocation) Discard
      modifying gameExplosions (\x -> x - 1)

  where
    validateInHand :: AppEff effs => Card -> Eff effs ()
    validateInHand card = do
      currentPlayerId <- view gameCurrentPlayer <$> get

      case view cardLocation card of
        Hand cardPlayerId | cardPlayerId == currentPlayerId -> return ()
        _ -> throwError "card is not in hand"

applyDiscard :: AppEff effs => CardId -> Eff effs ()
applyDiscard cid = do
  state <- get

  unless (view gameHints state < view gameMaxHints state) $
    throwError "cannot discard when at max hints"

  chosenCard <- requireCard cid

  currentPlayerId <- gets $ view gameCurrentPlayer

  modifying gameHints (\x -> x + 1)
  assign (gameCards . at cid . _Just . cardLocation) Discard

  maybeTopCard <- gets $
    headMaybe
      . filter (\card -> (view cardLocation card == Deck))
      . M.elems
      . view gameCards

  whenJust maybeTopCard $ \topCard ->
    -- Only draw if there are cards left in the deck
    assign
      (gameCards . at (view cardId topCard) . _Just . cardLocation)
      (Hand currentPlayerId)

applyHintRank :: AppEff effs => PlayerId -> Rank -> Eff effs ()
applyHintRank  = applyHint cardRank cardPossibleRanks

applyHintColor :: AppEff effs => PlayerId -> Color -> Eff effs ()
applyHintColor = applyHint cardColor cardPossibleColors

applyHint accessor possibleAccessor targetPid hint = do
  state <- get
  currentPlayerId <- gets $ view gameCurrentPlayer

  when (currentPlayerId == targetPid) $
    throwError "cannot hint self"

  unless (view gameHints state > 0) $
    throwError "insufficient hints remaining"

  let handCards =
        filter (\card -> view cardLocation card == Hand targetPid)
        . M.elems
        $ view gameCards state

  let (matchedCs, unmatchedCs) = Data.List.partition ((==) hint . view accessor) handCards

  unless (length matchedCs > 0) $
    throwError "can only hint ranks in hand"

  let cardAccessor =
        \card -> gameCards . at (view cardId card) . _Just . possibleAccessor

  forM_ matchedCs   $ \card -> assign    (cardAccessor card) (S.singleton hint)
  forM_ unmatchedCs $ \card -> modifying (cardAccessor card) (S.delete hint)

  modifying gameHints (\x -> x - 1)
