{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Network.EngineIO.Snap
import           Network.SocketIO

import           Snap
import           Snap.Util.FileServe

import           Backhand.Connection
import           Backhand.Game

main :: IO ()
main = do
    sockHandler <- initialize snapAPI connSockRoutes
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
