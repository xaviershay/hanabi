module Main where

import Network.Wai.Handler.Warp (run)
import Hanabi
import Hanabi.Types
import Hanabi.Api
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Time.Clock (UTCTime, getCurrentTime)
import           Control.Monad.IO.Class               (liftIO)

main :: IO ()
main = do
  let port = 8080
  state <- mkState
  now  <- liftIO getCurrentTime
--  putStrLn . show $ (encode . RedactedGame (PlayerId "Someone") . apply (Choice (PlayerId "Someone") (ChoiceHintColor (PlayerId "Xavier") Yellow)) $ mkGame now)
--  putStrLn . show $ (encode . RedactedGame (PlayerId "Someone") . apply (Choice (PlayerId "Someone") (ChoiceHintRank (PlayerId "Xavier") 2)) $ mkGame now)
--   putStrLn . show $ (encode . RedactedGame (PlayerId "Someone") . apply (Choice (PlayerId "Xavier") (ChoicePlayCard (CardId 2))) $ mkGame now)
  case applyWithInvalid (Choice (PlayerId "Xavier") (ChoiceDiscardCard (CardId 2))) $ mkGame now of
    Right game ->
      putStrLn . show $ (encode . RedactedGame (PlayerId "Someone")) game
    Left err -> putStrLn $ "ERROR: " <> err
  run port (app state)
