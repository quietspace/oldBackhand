-- | The `Core` is a data structure encapsulating a set of events and behaviors
-- which hold all of the game sessions for a particular type of game in
-- Backhand.
module Backhand.Core where

import           Data.Aeson
import           Data.Map      (Map)
import qualified Data.Map      as M
import qualified Data.Text     as T
import           FRP.Sodium

import           Backhand.Game


-- | Creates a game core for the given game.
mkCore :: Game
       -> Event LobbyId -- ^ Event which should fire to create a lobby with the
                        -- given ID.
       -> Reactive Core
mkCore game eNewLobby = do
    bLobbies' <- hold M.empty never
    return Core { coreGame = game
                , bLobbies = bLobbies'
                }


data Core = Core
    { coreGame :: Game -- ^ The game type object for this core.
    , bLobbies :: Behavior (Map LobbyId GameLobby) -- ^ A map of all of the
                                                     -- game lobbies.
    }

findLobby :: LobbyId -> Map LobbyId GameLobby -> Maybe GameLobby
findLobby = M.lookup


-- | A game lobby is a game which has not yet started.
data GameLobby = GameLobby
    { bMembers      :: Behavior [(Player, Bool)] -- ^ Members in this lobby. The
                                                 -- second element of the tuple
                                                 -- is True if the player is
                                                 -- playing.
    , eStartGame    :: Event Object -- ^ Starts the game with the given
                                    -- options.
    , bGameRoom     :: Behavior (Maybe GameRoom) -- ^ The game room if the game
                                                 -- is running.
    , pushNewMember :: Player -> Reactive () -- ^ Adds the given player as a
                                             -- spectator.
    }

type LobbyId = T.Text

