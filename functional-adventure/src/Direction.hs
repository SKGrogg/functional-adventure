module Direction where

data Direction 
    = N 
    | S 
    | E 
    | W
    deriving (Eq)

instance Show Direction where
    show :: Direction -> String
    show direction = 
        case direction of
            N -> "north"
            E -> "east"
            W -> "west"
            S -> "south"