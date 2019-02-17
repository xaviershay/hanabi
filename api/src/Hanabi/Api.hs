{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Hanabi.Api
  ( app
  , mkState
  ) where

import Hanabi.Types
import Hanabi.Apply

import           Network.Wai.Middleware.Cors          (cors, corsRequestHeaders,
                                                       simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           Control.Monad.Trans.Reader           (ReaderT, ask, runReaderT)
import qualified Data.HashMap.Strict                  as M
import           Data.Maybe                           (fromMaybe)
import qualified Data.Sequence                        as S
import qualified Data.Text                            as T
import           Control.Monad.STM                    (atomically, check, orElse)
import           Control.Concurrent                   (forkIO)
import           Control.Lens                         (over, view, set)
import           Control.Concurrent.STM.TVar          (TVar, modifyTVar,
                                                       newTVar, readTVar, registerDelay, writeTVar)
import           Control.Monad.IO.Class               (liftIO)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.ByteString.Lazy.Char8 as BSL

import Hanabi.Extras.RFC1123 (RFC1123Time(..))
import Hanabi.Extras.STM (readTVarWhen, Timeout(..))

type LastModifiedHeader = Header "Last-Modified" RFC1123Time

newtype State = State
  { games :: TVar (M.HashMap Int (TVar Game)) -- TODO: Use something like ttrie to avoid giant global TVar here.
  }

type AppM = ReaderT State Handler
type MyAPI =
  "_status"
    :> Get '[JSON] ()
  :<|> "games"
    :> ReqBody '[JSON] GameSpec
    :> Post '[JSON] Int
  :<|> "games"
    :> Capture "id" Int
    :> QueryParam' '[Required] "as" PlayerId
    :> QueryParam "version" Integer
    :> Get '[JSON] (Headers '[LastModifiedHeader] RedactedGame)
  :<|> "games"
    :> Capture "id" Int
    :> QueryParam' '[Required] "as" PlayerId
    :> ReqBody '[JSON] PlayerChoice
    :> Post '[JSON] ()

instance FromHttpApiData PlayerId where
  parseQueryParam x = PlayerId <$> parseQueryParam x

mkState :: IO State
mkState = do
  now  <- liftIO getCurrentTime
  x    <- atomically . newTVar $ mempty
  let s = State { games = x }
  addGame s 1 (mkGame now)
  return s


nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

api :: Proxy MyAPI
api = Proxy

app :: State -> Application
app s =   logStdoutDev
        $ cors (const . Just $ corsPolicy)
        $ serve api
        $ hoistServer api (nt s) server
  where
    corsPolicy = simpleCorsResourcePolicy
                   { corsRequestHeaders = [ "authorization", "content-type" ]
                   }

server = getStatus :<|> postGame :<|> getGame :<|> postChoice

getStatus :: AppM ()
getStatus = return ()

postGame :: GameSpec -> AppM Int
postGame spec = do
  s@State{games = stateVar} <- ask
  gameMap <- liftIO . atomically . readTVar $ stateVar

  let newId = maximum (0:M.keys gameMap) + 1

  now  <- liftIO getCurrentTime
  liftIO $ addGame s newId (mkGame now)

  return newId

getGame :: Int -> PlayerId -> Maybe Integer -> AppM (Headers '[LastModifiedHeader] RedactedGame)
getGame gameId requestingPlayer maybeVersion = do
  let currentVersion = fromMaybe 0 maybeVersion

  gvar <- findGame gameId
  g <- liftIO $ readTVarWhen
                  gvar
                  (\x -> view gameVersion x > currentVersion)
                  (TimeoutSecs 30)
                >>= maybe (atomically . readTVar $ gvar) return

  return
    . addHeader (RFC1123Time . view gameModified $ g)
    $ RedactedGame requestingPlayer g

postChoice :: Int -> PlayerId -> PlayerChoice -> AppM ()
postChoice gameId choosingPlayer choice = do
  gvar <- findGame gameId

  result <- liftIO $ do
    now  <- getCurrentTime

    atomically $ do
      state <- readTVar gvar

      case apply (Choice choosingPlayer choice) state of
        Right newGame -> do
          writeTVar gvar
            . over gameVersion (+ 1)
            . set gameModified now
            $ newGame
          return (Right ())
        Left err -> return (Left err)

  case result of
    Right () -> return ()
    Left err -> throwError err400 { errBody  = BSL.pack err }

findGame :: Int -> AppM (TVar Game)
findGame gameId = do
  State{games = stateVar} <- ask

  gameMap <- liftIO . atomically . readTVar $ stateVar

  case M.lookup gameId gameMap of
    Nothing -> throwError err404
    Just gvar -> return gvar

addGame :: State -> Int -> Game -> IO ()
addGame State { games = stateVar } gameId g = do
  gvar <- atomically . newTVar $ g
  atomically . modifyTVar stateVar $
    -- TODO: Check not overwriting an existing
    M.insert gameId gvar

  forkIO $ handleExpiry gvar

  return ()

  where
    -- Remove the game from state if a timeout expires without any change to
    -- the game's modification time.
    handleExpiry :: TVar Game -> IO ()
    handleExpiry gvar = do
      g <- atomically . readTVar $ gvar
      let currentModified = view gameModified g
      mres <- readTVarWhen
                gvar
                ((/=) currentModified . view gameModified)
                (TimeoutDays 7)
      case mres of
        Nothing -> atomically . modifyTVar stateVar $
                     M.delete gameId
        Just newGame -> handleExpiry gvar
