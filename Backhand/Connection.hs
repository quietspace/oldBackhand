{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Backhand.Connection where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.Maybe
import qualified Data.Text                  as T
import           FRP.Sodium
import           Network.SocketIO

import           Backhand.Core
import           Backhand.Game


type MonadSocket m = (Applicative m, MonadIO m, MonadReader Socket m)

type SockRoutingT m = StateT RoutingTable (ReaderT Socket m)


connSockRoutes :: (Applicative m, MonadIO m) => Core -> SockRoutingT m ()
connSockRoutes core = do
    eMsg <- onEvent "msg"
    (bGameLobbyM, eErrNoLobby) <- behGameLobby core

    let eError = const "Lobby does not exist." <$> eErrNoLobby

    socketListen eMsg echoMsg
    socketListen eError $ \msg ->
        emit "error" (msg :: T.Text)


-- | Handles "lobby" messages.
-- Returns a behavior containing the current lobby (if set) and an event which
-- fires if the client tries to set a lobby which doesn't exist.
behGameLobby :: (MonadIO m, Applicative m) => Core
             -> SockRoutingT m (Behavior (Maybe GameLobby), Event ())
behGameLobby core = do
    eSetLobbyId <- once <$> onEvent "lobby"

    let eSetLobbyM = snapshot findLobby eSetLobbyId $ bLobbies core
        eSetLobby = filterJust eSetLobbyM
        eErrorNoLobby = const () <$> filterE isNothing eSetLobbyM

    bGameLobby <- liftIO $ sync $ hold Nothing $ fmap Just eSetLobby
    return (bGameLobby, eErrorNoLobby)


echoMsg :: T.Text -> EventHandler ()
echoMsg msg = do
    liftIO $ putStrLn $ T.unpack msg
    emit "msg" msg



-- | Creates an FRP `Event` with Socket.io events with a given ID.
onEvent :: (Applicative m, MonadIO m, FromJSON a) => T.Text
        -> SockRoutingT m (Event a)
onEvent evtId = do
    (eEvent, pushEvt) <- liftIO $ sync newEvent
    on evtId (liftIO . sync . pushEvt)
    return eEvent

-- | Like Sodium's `listen`, but runs a `MonadSocket` action instead of an IO
-- action.
-- No cleanup function is needed, as cleanup will be handled automatically when
-- the socket disconnects.
socketListen :: (MonadSocket m, MonadState RoutingTable m) =>
                Event a -> (a -> EventHandler ()) -> m ()
socketListen evt h = do
    socket <- ask
    cleanup <- liftIO $ sync $ listen evt (\a -> runReaderT (h a) socket)
    appendDisconnectHandler $ liftIO cleanup

