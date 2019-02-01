module Main where

import Network.Wai.Handler.Warp (run)
import Hanabi
import Hanabi.Types
import Hanabi.Api
import Data.Aeson
import Data.Time.Clock (UTCTime, getCurrentTime)
import           Control.Monad.IO.Class               (liftIO)

main :: IO ()
main = do
  let port = 8080
  state <- mkState
  now  <- liftIO getCurrentTime
  putStrLn . show $ (encode . apply (Choice (PlayerId 456) (ChoiceHintRank (PlayerId 123) 2)) $ mkGame now)
  run port (app state)
