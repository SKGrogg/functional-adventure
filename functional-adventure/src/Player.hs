module Player where

import Item
import Room

data Player = Player 
    {   inventory :: [ItemName],
        maxWeight :: Integer,
        location :: RoomName
    }
    deriving (Show, Eq)

--Adds an item to a player's inventory
addItem :: ItemName -> Player -> Player
addItem newItem player = player { inventory = (newItem : (inventory player))}

--Removes an item from a player's inventory
removeItem :: ItemName -> Player -> Player
removeItem remItem player = player 
                                { inventory = (filter helper (inventory player))}
    where 
        helper item = 
            if item == remItem
                then False
                else True

--Changes the location of a player
newLocation :: RoomName -> Player -> Player
newLocation newRoom player = player { location = newRoom}

--Checks to see if the player has any objects in their inventory
isCarryingAnything :: Player -> Bool
isCarryingAnything player = 
    if null (inventory player)
        then False
        else True

--Player for Level One
youOne :: Player
youOne = Player [] 150 EntryHall

--Player for Level Two
youTwo :: Player
youTwo  = Player [] 150 CommandDeck