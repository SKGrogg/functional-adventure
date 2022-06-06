{-# OPTIONS_GHC -Wno-unused-imports #-}
module GameState where

import Data.List
import Control.Exception
import qualified Data.Map as M
import qualified Data.Either as E

import Item
import Room
import Player
import Direction

type GameMap = M.Map RoomName Room

data GameState = GameState
    {
        message :: Maybe String,
        gmap :: GameMap,
        universe :: Universe,
        player :: Player,
        level :: Integer
    }
    deriving (Show)

type Error a = Either String a
mkMap :: [Room] -> GameMap
mkMap rooms = 
    M.fromList helper 
    where
        helper = fmap (\room@Room {rname}-> (rname, room)) rooms

--GameMap for Level One
gameMapOne :: GameMap
gameMapOne = mkMap allRoomsOne

--GameMap for Level Two
gameMapTwo :: GameMap
gameMapTwo = mkMap allRoomsTwo

--Initial GameState to start the game
initialState :: GameState
initialState = GameState Nothing gameMapOne univOne youOne 1

data KeyError = KeyError
  deriving Show

instance Exception KeyError

--Helper function to get an object
getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

--Returns an object using only its name
getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)

--Helper Function to get a room
getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

--Gets a room using only its name
getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)

--Allows you to add a new room to the GameMap
setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap name room gamemap = M.insert name room gamemap

--Set a new message for the GameState
setMessage :: String -> GameState -> GameState
setMessage str gmst = 
    if str == ""
        then gmst {message = Nothing}
        else gmst {message = Just str}

--THREE NEW FUNCTIONS I ADDED

--Set a whole new map for the GameState upon level up
setNewMap :: GameMap -> GameState -> GameState
setNewMap mp gmst = gmst {gmap = mp}

--Set a whole new universe for the GameState upon level up
setNewUniv :: Universe -> GameState -> GameState
setNewUniv un gmst = gmst {universe = un}

--Set a whole new player for the GameState upon level up
setNewPlayer :: Player -> GameState -> GameState
setNewPlayer pl gmst = gmst {player = pl}

--Set a whole new universe for the GameState upon level up
setNewLevel :: GameState -> GameState
setNewLevel gmst = gmst {level = 2}

--Returns a list of the player's currently held items
currentInventory :: GameState -> [ItemName]
currentInventory gmst = inventory $ player gmst

--Returns the room the player is currently located in
currentRoom :: GameState -> Room
currentRoom gmst = getRoom (location (player gmst)) gmst

--Returns items that are in the current room
nearbyObjects :: GameState -> [ItemName]
nearbyObjects gmst = objects $ currentRoom gmst

--Checks to see what the current sum of all weights in inventory is
inventoryWeight :: GameState -> Integer
inventoryWeight gmst = do
    let inv = inventory (player gmst)
    let helper gmst' a' = weight (getObject a' gmst')
    foldl (\acc a -> acc + (helper gmst a)) 0 inv
           
--Checks to see if user is trrying to pick up an item they are holding
alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
alreadyHaveTakeCheck it gmst = do 
    let inv = inventory (player gmst)
    let b = foldl (\acc a -> (a == it) || acc) False inv
    if b
        then Left (("You are already carrying the " ++ (show it)) ++ ".")
        else Right gmst

--checks to see if the object the user is trying to take is in the room already
inRoomTakeCheck :: ItemName -> GameState -> Error GameState
inRoomTakeCheck it gmst = do 
    let possObjs = nearbyObjects gmst
    let b = foldl (\acc a -> (a == it) || acc) False possObjs
    if (not b)
        then Left (("There is no " ++ (show it)) ++ " in this room.")
        else Right gmst

--Checks to see if the uuser can pick uup the new item
weightCheck :: ItemName -> GameState -> Error GameState
weightCheck it gmst = do
    let addWeight = weight (getObject it gmst)
    let currWeight = inventoryWeight gmst
    let maxW = maxWeight (player gmst)
    if (addWeight + currWeight > maxW)
        then Left "That's too much weight for you to carry."
        else Right gmst

--Checks to if the user is trying to interact with an item not currently present
anywhereDropCheck :: ItemName -> GameState -> Error GameState
anywhereDropCheck it gmst = do
    let eOne = alreadyHaveTakeCheck it gmst
    let eTwo = inRoomTakeCheck it gmst
    if ((E.isLeft eOne) || (E.isRight eTwo))
        then Right gmst
        else Left (("What do you mean, drop the " ++ (show it)) ++ "?")

--Checks to see if user is trying to drop an item they don't have
inRoomDropCheck :: ItemName -> GameState -> Error GameState
inRoomDropCheck it gmst = do 
    let possObjs = nearbyObjects gmst
    let b = foldl (\acc a -> (a == it) || acc) False possObjs
    if b
        then Left (("You aren't carrying the " ++ (show it)) ++ ".")
        else Right gmst

--Checks to see if the current room has any oobjects
roomHasObjects :: GameState -> Bool
roomHasObjects gmst = hasObjects $ currentRoom gmst

--Returns the name of the room the player is traveling
destinationName :: Direction -> Room -> Maybe RoomName
destinationName dir rm = do 
    let exs = exits rm
    let filt = (\(k,v) -> k == dir)
    let res = filter filt exs
    if res == []
        then Nothing
        else Just (snd $ res !! 0)

--Changes the GameState to reflect the player's new location
move :: Direction -> GameState -> GameState
move dir gmst = do
    let currRm = currentRoom gmst
    case destinationName dir currRm of
        Just rm ->
            let newPlayer = (player gmst) {location = rm}
                newMessage = Just (("You go " ++ show dir) ++ ".")
                in gmst {message = newMessage,
                        player = newPlayer}
        Nothing ->
            let newMessage = Just "There is no exit in that direction."
                in gmst {message = newMessage}

--Checks to see if the user has won their current level
--Trigger if pulsing orb brought to tower and bathing suit to infinity pool
haveWonGame :: GameState -> Bool
haveWonGame gmst = 
    if ( ((rname $ currentRoom gmst) == Tower) && 
        (E.isLeft (alreadyHaveTakeCheck PulsingOrb gmst)) ) 
        || 
        (((rname $ currentRoom gmst) == InfinityPool) && 
        (E.isLeft (alreadyHaveTakeCheck BathingSuit gmst)))
        then True
        else False

----Changes the GameState to reflect player picking up an item
takeItem :: ItemName -> GameState -> GameState
takeItem itName gmst = do
    let errorOne = alreadyHaveTakeCheck itName gmst
    let errorTwo = inRoomTakeCheck itName gmst
    let errorThree = weightCheck itName gmst
    case errorOne of
        Left mess -> gmst {message = Just mess}
        _ ->
            case errorTwo of
                Left mess' -> gmst {message = Just mess'}
                _ -> 
                    case errorThree of
                        Left mess'' -> gmst {message = Just mess''}
                        _ -> let newroom  = Room.removeItem itName (currentRoom gmst) in
                                gmst { message = Just (("You take the " ++ (show itName)) ++ "."),
                                        gmap = setRoomMap (rname $ currentRoom gmst) newroom (gmap gmst),
                                        player = Player.addItem itName (player gmst) }

--Changes the GameState to reflect the player dropping an item
dropItem :: ItemName -> GameState -> GameState
dropItem itName gmst = do
    let errorOne = anywhereDropCheck itName gmst
    let errorTwo = inRoomDropCheck itName gmst
    case errorOne of
        Left mess -> gmst {message = Just mess}
        _ ->
            case errorTwo of
                Left mess' -> gmst {message = Just mess'}
                _ -> let newroom  = Room.addItem itName (currentRoom gmst) in
                        gmst { message = Just ("You drop the " ++ (show itName)),
                                gmap = setRoomMap (rname $ currentRoom gmst) newroom (gmap gmst),
                                player = Player.removeItem itName (player gmst) }

--Let's the user know how much weight a given item in their inventory weighs
viewItem ::  ItemName -> GameState -> GameState
viewItem itName gmst = do 
    let errorOne = alreadyHaveTakeCheck itName gmst
    case errorOne of
        Right st -> st {message  = Just (("You are not carrying the " ++ (show itName)) ++ ".")}
        Left mess -> let itWeight = weight $ getObject itName gmst in
                    gmst {message  = Just (((("The " ++  (show itName)) ++ " weighs ") ++ (show itWeight)) ++ ".")}

