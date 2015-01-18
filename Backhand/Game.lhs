This module is responsible for defining the interface between backhand and
games.

> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
> -- | This module defines the interface between backhand and a game.
> module Backhand.Game where
> 
> import           Data.Aeson
> import qualified Data.Text as T
> import           FRP.Sodium


The Game
========

Any particular game in backhand is represented as an instance of the `Game` data
structure. This data structure holds information about a game, such as its name
and a function to create a room for that game.

> -- | Data structure which represents a particular game.
> -- Contains info such as the name and a function to create a room.
> data Game = Game
>     { gameName :: T.Text -- ^ The name of this game.
>       -- | Constructs a new room for the game. The game should start as soon
>       -- as this is done.
>     , newGame :: [Player]   -- ^ A list of players in the game.
>               -> Object     -- ^ JSON object with game options.
>               -> Event Move -- ^ Event stream of moves made.
>               -> GameRoom
>       -- TODO: Implement a better game options system.
>       -- TODO: Handle dropping players.
>     }


The Game Room
=============

When a game is started, its `newGame` function is called, which sets up the
game's events and behaviors and then returns a `GameRoom` object.  This data
structure contains all of the events and behaviors pertaining to a particular
game lobby.

> -- | Data structure which holds events and behaviors for a particular game
> -- room or lobby.
> data GameRoom = GameRoom
>     {
>     -- | @True@ if players should be allowed to join the given game in its
>     -- current state.
>       bJoinAllowed :: Behavior Bool
>
>     -- | If applicable, this will be a behavior which contains the player ID
>     -- of the player whose turn it is.
>     , bCurrentTurn :: Behavior (Maybe MemberId) 
>
>     -- | Event which should fire to send some data to a player.
>     -- The second element of the tuple is a list of player IDs to send the
>     -- data to. If it is empty, the data is sent to all players.
>     , eGameUpdate :: Event (Update, [MemberId])
>     }


Moves
=====

Moves are represented by a move ID, a player ID, and a JSON object with
data about the move.

> -- | Data structure for moves made by players.
> -- This is basically a type which is used to encapsulate messages sent by
> -- players to the game.
> data Move = Move
>     { moveType   :: MoveType -- ^ Indicates what type of move this is.
>     , moveData   :: Object   -- ^ JSON object with information about the
>                              -- move.
>     , movePlayer :: MemberId -- ^ ID of the player making the move.
>     }
> 
> type MoveType = T.Text


Updates
=======

Game updates act as a counterpart to moves. They are used to update a player
when something in the game changes. Updates are implemented by the `Update` type
and are composed of an update type name and some JSON data.

> -- | Data structure for sending data about game state changes.
> -- This is the counterpart to the `Move` data structure. It is used by the
> -- game to send data to players.
> data Update = Update
>     { updateType :: UpdateType -- ^ Indicates what type of update this is.
>     , updateData :: Object     -- ^ JSON object with update data.
>     }
> 
> type UpdateType = T.Text


Players
=======

Players in the game are represented to the game by `Player` data structures.
The game data type is responsible for managing player-related state, while
Backhand is responsible for managing the player's connection.

The `Player` data type contains a few fields, but each one can be uniquely
identified within a game instance by its `MemberId` field. (See the "Members"
section below for more info on `MemberId`s.)

> -- | Data type for representing a player in a game.
> data Player = Player
>     { playerId :: MemberId -- ^ Unique internal ID for the player.
>     , playerName :: T.Text -- ^ The player's display name.
>     }
> 
> instance Member Player where
>     memberId = playerId


Members
=======

A member in a game is someone who is either a spectator or a player. All members
have a `MemberId` which is unique to that member (within the game, at least).

> -- | Class for members.
> class Member mem where
>     memberId :: mem -> MemberId -- ^ Gets the member's ID.
> 
> type MemberId = Integer
