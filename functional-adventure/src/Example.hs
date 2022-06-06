{-# OPTIONS_GHC -Wno-unused-imports #-}
module Example where

import Data.List
import System.Random


import Item
import Direction
import Room
import Player
import GameState
import Data.IntMap (assocs, toList)
import qualified Data.Foldable as M

--Returns a list of IO a's the length of the second input
exampleList :: IO a -> IO Int -> IO [a]
exampleList side1 side2 = do
    
    len <- side2
    let ioList = replicate len $ side1

    sequence ioList

--Chooses a random element from a list
choose :: [a] -> IO a
choose xs = do
    let end = (length xs) -1
    ind <- randomRIO(0, end)
    pure $ xs !! ind
    

class Example a where
    example :: IO a


instance Example Item where
    example :: IO Item
    example = do
        
        iName' <- choose itemNamesOne 
        weight' <- randomRIO (1,100)
        pure $ Item iName' weight'

instance Example Direction where
    example :: IO Direction 
    example = do

        let options  = [N, S, E, W]
        choose options

--Creates an example of an exit tuple from existing directions and rooms
exitExample :: IO Exit 
exitExample = do

    dir <- choose [N, S, E, W]
    room <- choose roomNames 
    pure $ (dir, room)

--Creates an example room, mostly used for testing
instance Example Room where
    example :: IO Room 
    example = do

        roomName <- choose roomNames 
        let description = "You are in a randomly-generated room, which is the " ++ (show roomName)
        exits <- exampleList exitExample (randomRIO (2,4))
        objects <- exampleList (choose itemNamesOne) (randomRIO (2,5))
        pure $ Room roomName description exits objects

--Creates an example player, mostly used for testing
instance Example Player where
    example :: IO Player 
    example = do
        inventory <- exampleList (choose itemNamesOne) (randomRIO (0,10))
        maxWeight <- (randomRIO (800, 900))
        roomName <- choose roomNames 
        pure $ Player inventory maxWeight roomName

--Creates an example GameState, mostly used for testing
instance Example GameState where
    example :: IO GameState
    example = do 
        let possMessages = [Just "One possible message.", Just "Yet another possible message.", Nothing]
        message <- choose possMessages
        roomList <- exampleList (example :: IO Room) (randomRIO(2,3) :: IO Int)
        let gamemap = mkMap roomList
        itemList <- exampleList (example :: IO Item) (randomRIO(5,10) :: IO Int)
        let uni = mkUniverse itemList
        playa <- example :: IO Player
        pure $ GameState message gamemap uni playa 1
