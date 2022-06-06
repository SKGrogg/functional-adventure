module GameIO where

import Control.Monad.State
import System.Exit
import System.IO
import GameState
import Player
import Room
import Command
import Item

type GameIO t = StateT GameState IO t

--Allows us to alter the GameState in a way 
--that allows the persists through the game
effectChange :: (GameState -> GameState) -> GameIO ()
effectChange = modify 

--Adds a line to show the user they should type
prompt :: GameIO ()
prompt =  lift $ putStrLn "-> "

--Prints the current message of the GameState, 
--and resets to Nothing after printing
printMessage :: GameIO ()
printMessage = do
    s <- get

    case message s of
        Just msg -> do
            effectChange (setMessage "")
            lift $ putStrLn msg
        Nothing -> pure ()

--Prints the description of the current room
printDescription :: GameIO ()
printDescription = do
    s <- get
    let r = currentRoom s

    lift $ putStrLn $ desc r

--Prints the object found in the current room
printObjects :: GameIO ()
printObjects = do 
    s <- get
    
    let r = currentRoom s 
    let os = objects r 

    case os of 
        [] -> pure ()
        _ -> do
            lift $ putStrLn "You see the following objects:"

            void $ mapM (\itemName -> lift $ putStrLn $ show itemName) os

--Prints the directions of the exits of the current room
printExits :: GameIO ()
printExits = do
    s <- get

    let r = currentRoom s 
    let exs = exits r

    case exs of
        [] -> pure ()
        _ -> do
            lift $ putStrLn "There are exits in the following directions:"
            void $ mapM (\(dir, rm) -> lift $ putStrLn $ show dir) exs

    
--Prints the players current inventory
printInventory :: GameIO ()
printInventory = do
    s <- get
    
    let inv = currentInventory s 

    case inv of
        [] -> lift $ putStrLn "You aren't carrying anything."
        _ -> do
            lift $ putStrLn "You are carrying the following objects:"
            void $ mapM (\itemName -> lift $ putStrLn $ show itemName) inv

--Performs a number of take/drop commands regarding a list of items
actionOverList :: (ItemName -> GameState -> GameState) -> [ItemName] -> GameIO ()
actionOverList func lst = do

    void $ mapM (\it -> effectChange (func it) >> printMessage) lst


--Print success and exit when game is won
finishGame :: GameIO ()
finishGame =
     lift $ putStrLn ("You brought the bathing suit to the infinity pool. " ++ 
                "Time to kick back, take a dip, and bask in the cosmos!") >> 
                    putStrLn "Congrats! You win!" >> exitSuccess

--Set Game to Level Two 
levelUp :: GameIO ()
levelUp = do
    let newMess = "Gad zooks! That orb was a homing beacon and you've been whisked "
                    ++ "out of the dark ages and on board a space ship! " ++
                        "Your primitive mind can hardly grasp your strange new surroundings."
    void $ effectChange (setMessage newMess) >>
            effectChange (setNewMap gameMapTwo) >> 
                effectChange (setNewUniv univTwo) >>
                    effectChange (setNewLevel) >>
                        effectChange (setNewPlayer youTwo) >>
                            printMessage
                    
--Allows player to quit game
exit :: GameIO ()
exit = 
    lift $ putStrLn "Goodbye!" >> exitSuccess

--Checks if player has defeated level/game
checkGameOver :: GameIO ()
checkGameOver = do
    s <- get
    case haveWonGame s of
        False -> pure ()
        True -> do
            case (level s) of
                2 -> finishGame
                _ -> levelUp
    
        
--Alerts the user to an invalid Input
syntaxError :: GameIO ()
syntaxError = 
    lift $ putStrLn "I didn't understand that."

--Greets new user
opening :: GameIO ()
opening = 
    lift $ (putStrLn "Welcome to Functional Adventure!" >>
                        putStrLn ("1153 C.E.: You are a peasant trapped in an abandoned castle."
                                    ++ " How will you find your way out?"))

--Alters GameState accoridng to command
performCommand :: Command -> GameIO ()
performCommand cmmd = 
    case cmmd of
        Look -> printDescription >> printObjects >> printExits
        Move dir -> effectChange (move dir) >> printMessage
        Inventory -> printInventory
        Take lst -> actionOverList takeItem lst
        Drop lst -> actionOverList dropItem lst
        View it -> effectChange (viewItem it) >> printMessage
        Exit -> exit

--Performs a series of commands
performConjunction :: Conjunction -> GameIO ()
performConjunction lst = 
    void $ mapM (\cmmd -> performCommand cmmd) lst

--Breaks up user input into commands
parseConjunction :: String -> GameIO ()
parseConjunction str = 
    case parseInput str of
        Just conj -> performConjunction conj
        Nothing -> syntaxError

--Allows for ser interation with game
repl :: GameIO ()
repl = do
    prompt
    inp <- lift getLine
    parseConjunction inp
    checkGameOver