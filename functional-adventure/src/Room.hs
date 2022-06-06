module Room where

import Item
import Direction
import Data.List

data RoomName
  = EntryHall
  | GreatHall
  | Tower
  | ThroneRoom
  | Dungeon
  | CommandDeck
  | EngineRoom
  | InfinityPool
  deriving (Eq, Ord)

instance Show RoomName where
    show :: RoomName -> String
    show EntryHall = "entry hall"
    show GreatHall = "great hall"
    show Tower = "tower"
    show ThroneRoom = "throne room"
    show Dungeon = "dungeon"
    show CommandDeck = "command deck"
    show EngineRoom = "engine room"
    show InfinityPool = "infinity pool"

type Exit = (Direction, RoomName)

data Room = Room
    {   rname :: RoomName,
        desc :: String,
        exits :: [Exit],
        objects :: [ItemName]
    }
    deriving (Show, Eq)

entryHall :: Room
entryHall = Room 
                EntryHall "You find yourself in an oppulent, torchlit medieval entry hall, " 
                    [(N, ThroneRoom), (E, GreatHall), (S, Tower)] [Bucket, JesterOutfit]
greatHall :: Room
greatHall = Room 
                GreatHall "You enter the Great Hall, tables set for a feast but empty." 
                    [(W, EntryHall)] [Wand, Potion]
tower :: Room
tower = Room 
                Tower "You ascend a spire, arriving atop a tower that looks across the kingdom." 
                    [(N, EntryHall)] [ProphecyScroll]
throneRoom :: Room
throneRoom = Room 
                ThroneRoom ("You enter the throne room." 
                    ++"Upon the throne sits a mighty broadswoard and a hefty shield." )
                        [(S, EntryHall), (N, Dungeon)] [Broadsword, Shield]
dungeon :: Room
dungeon = Room 
                Dungeon "You are in the dugeon. It smells of witchcraft here." 
                    [(S, ThroneRoom)][BathingTub, PulsingOrb]
commandDeck :: Room
commandDeck = Room 
                CommandDeck ("You are on the command deck of a spacefaring vessel." 
                    ++ "The ship is set to autopilot. Solar systems whizz by.")
                        [(E, EngineRoom), (W, InfinityPool)][UniversalTranslator, RayGun, SpaceFood]
engineRoom :: Room
engineRoom = Room 
                EngineRoom ("You are in the engine room of the ship." 
                    ++ "You cannot begin to comprehend the technology.")
                    [(W, CommandDeck)][BathingSuit]
infinityPool :: Room
infinityPool = Room 
                    InfinityPool ("You enter a room with an infinity pool." 
                        ++"The water seems to spill out into the cosmos itself.") 
                            [(E, CommandDeck)][Tamagotchi]

--A List of all Level One RoomNames, used for Examples
roomNames :: [RoomName]
roomNames = foldr (\a acc -> (rname a):acc) [] allRoomsOne

--All Rooms in Level One
allRoomsOne :: [Room]
allRoomsOne = [entryHall, throneRoom, greatHall, dungeon, tower]

--All Rooms in Level Two
allRoomsTwo :: [Room]
allRoomsTwo = [commandDeck, engineRoom, infinityPool]

--Adds an item to a Room's object list
addItem :: ItemName -> Room -> Room
addItem item room = room { objects = item :(objects room) }

--Removes an item from a room's object list
removeItem :: ItemName -> Room -> Room
removeItem item room = room { objects = (Data.List.delete item (objects room))}

--Checks to see if there are any objects in the room
hasObjects :: Room -> Bool
hasObjects rm = 
    if (objects rm) == []
        then False
        else True

