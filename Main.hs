{-# LANGUAGE OverloadedStrings #-}
module Main where


import           FRP.Sodium
import           Network.EngineIO.Snap
import           Network.SocketIO

import           Snap
import           Snap.Util.FileServe

import           Backhand.Connection
import           Backhand.Core
import           Backhand.Game

testGame :: Game
testGame =
    Game { gameName = "Test Game"
         , newGame = undefined
         }

main :: IO ()
main = do
    core <- sync $ mkCore testGame never
    sockHandler <- initialize snapAPI $ connSockRoutes core
    quickHttpServe $ site sockHandler


site :: Snap () -> Snap ()
site sockHandler =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("/socket.io", sockHandler)
          ] <|>
    serveDirectory "static"

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "msg"
    maybe (writeBS "must specify message") writeBS param
