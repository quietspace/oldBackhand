{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Backhand.Connection where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Aeson
import qualified Data.Text                  as T
import           Network.SocketIO

import           Backhand.Game


type MonadSocket m = (Applicative m, MonadIO m, MonadReader Socket m)

connSockRoutes :: (Applicative m, MonadIO m) =>
                  StateT RoutingTable (ReaderT Socket m) ()
connSockRoutes = do
    on "msg" echoMsg
    return ()

echoMsg :: MonadSocket m => T.Text -> m ()
echoMsg msg = do
    liftIO $ putStrLn $ T.unpack msg
    emit "msg" msg
