module Main where

import Control.Monad.State

import GameState
import GameIO

--Main Driver for the program. Gets the game started
main :: IO ()
main = do
    evalStateT (opening >> forever repl) initialState

