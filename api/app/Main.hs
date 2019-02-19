module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy     as BSL
import           Data.Time.Clock          (UTCTime, getCurrentTime)
import           Hanabi
import           Hanabi.Api
import           Hanabi.Types
import           Network.Wai.Handler.Warp (run)
import           System.Random (newStdGen)

main :: IO ()
main = do
  let port = 8080
  state <- mkState
  now  <- liftIO getCurrentTime
  rng  <- liftIO newStdGen
--  putStrLn . show $ (encode . RedactedGame (PlayerId "Someone") . apply (Choice (PlayerId "Someone") (ChoiceHintColor (PlayerId "Xavier") Yellow)) $ mkGame now)
--  putStrLn . show $ (encode . RedactedGame (PlayerId "Someone") . apply (Choice (PlayerId "Someone") (ChoiceHintRank (PlayerId "Xavier") 2)) $ mkGame now)
--   putStrLn . show $ (encode . RedactedGame (PlayerId "Someone") . apply (Choice (PlayerId "Xavier") (ChoicePlayCard (CardId 2))) $ mkGame now)
  let game = setupGame [PlayerId "Xavier", PlayerId "Jared"] $ mkGame rng now
  BSL.putStrLn $ (encode . RedactedGame (PlayerId "Someone")) game
  run port (app state)
