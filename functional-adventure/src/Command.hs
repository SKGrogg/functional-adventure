module Command where

import Text.Parsec hiding (parse, runParser, (<|>), sepBy1, sepBy, choice)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Data.Char
import Data.List

import Item
import Direction

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . map try

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

parse :: Parser a -> String -> Either ParseError a
parse parser = P.parse parser ""

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | View ItemName
  | Exit
  deriving (Eq, Show)

type Conjunction = [Command]

itemNameP :: Parser ItemName
itemNameP = 
    choice 
        [Broadsword <$ string "broadsword",
            JesterOutfit <$ string "jester's outfit",
            BathingTub <$ string "bathing tub",
            Bucket <$ string "bucket",
            PulsingOrb <$ string "pulsing orb",
            Wand <$ string "wand",
            Shield <$ string "shield",
            ProphecyScroll <$ string "prophecy scroll",
            Potion <$ string "potion",
            UniversalTranslator <$ string "universal translator",
            RayGun <$ string "ray gun",
            SpaceFood <$ string "space food",
            Tamagotchi <$ string "tamagotchi",
            BathingSuit <$ string "bathing suit"
        ]

nounPhrase_stub :: Parser [ItemName]
nounPhrase_stub = do 
    i <- (sepBy1 itemNameP (spaces *> char ',' <* spaces))
    pure $ i

nounPhrase :: Parser [ItemName]
nounPhrase = nounPhrase_stub

inventoryP :: Parser Command
inventoryP = Inventory<$ string "inventory"

takeP :: Parser Command
takeP = do
    items <- string "take " *> nounPhrase
    pure $ Take items

exitP :: Parser Command
exitP = do  
    exitCheck <- choice [string "quit", string "exit"]
    pure $ Exit

dropP :: Parser Command
dropP = do
    items <- string "drop " *> nounPhrase
    pure $ Drop items

lookP :: Parser Command
lookP = do  
    lookCheck <- string "look"
    pure $ Look

directionP :: Parser Direction
directionP = 
    choice 
        [N <$ string "north",
            S <$ string "south",
            E <$ string "east",
            W <$ string "west"
        ]

moveP :: Parser Command
moveP = do
    dir <- directionP
    pure $ Move dir

viewP :: Parser Command
viewP = do
    item <- string "view " *> itemNameP
    pure $ View item

commandP :: Parser Command
commandP = 
    choice 
        [inventoryP,
        takeP,
        exitP,
        dropP,
        lookP,
        moveP, 
        viewP
        ]

conjunctionP :: Parser Conjunction
conjunctionP = do
    commands <- (sepBy1 commandP (string " and ")) <* eof
    pure $ commands
    
parseInput :: String -> Maybe Conjunction
parseInput str = 
    case parse conjunctionP str of
        Right ans -> Just ans
        Left err -> Nothing