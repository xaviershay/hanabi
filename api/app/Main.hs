module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy     as BSL
import           Data.Time.Clock          (UTCTime, getCurrentTime)
import           Hanabi
import           Hanabi.Api
import           Hanabi.Types
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  let port = 8080
  state <- mkState
  now  <- liftIO getCurrentTime
--  putStrLn . show $ (encode . RedactedGame (PlayerId "Someone") . apply (Choice (PlayerId "Someone") (ChoiceHintColor (PlayerId "Xavier") Yellow)) $ mkGame now)
--  putStrLn . show $ (encode . RedactedGame (PlayerId "Someone") . apply (Choice (PlayerId "Someone") (ChoiceHintRank (PlayerId "Xavier") 2)) $ mkGame now)
--   putStrLn . show $ (encode . RedactedGame (PlayerId "Someone") . apply (Choice (PlayerId "Xavier") (ChoicePlayCard (CardId 2))) $ mkGame now)
  case apply (Choice (PlayerId "Xavier") (ChoiceDiscardCard (CardId 2))) $ mkGame now of
    Right game ->
      BSL.putStrLn $ (encode . RedactedGame (PlayerId "Someone")) game
    Left err -> putStrLn $ "ERROR: " <> err
  run port (app state)
