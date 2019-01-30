module Main where

import Network.Wai.Handler.Warp (run)
import Hanabi

main :: IO ()
main = do
  let port = 8080
  state <- mkState
  run port (app state)
