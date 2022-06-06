module Item where

import qualified Data.Map as M

data ItemName
  = Broadsword
  | JesterOutfit
  | BathingTub
  | Bucket
  | Wand
  | PulsingOrb
  | Shield
  | ProphecyScroll
  | Potion
  | UniversalTranslator
  | RayGun
  | SpaceFood
  | BathingSuit
  | Tamagotchi
  deriving (Eq, Ord)

data Item = Item
    {   iname :: ItemName,
        weight :: Integer
    }
    deriving (Show, Eq)

instance Show ItemName where
    show :: ItemName -> String
    show Broadsword = "broadsword"
    show JesterOutfit = "jester's outfit"
    show BathingTub = "bathing tub"
    show Bucket = "bucket"
    show PulsingOrb = "pulsing orb"
    show Wand = "wand"
    show Shield = "shield"
    show ProphecyScroll = "prophecy scroll"
    show Potion = "potion"
    show UniversalTranslator = "universal translator"
    show RayGun = "ray gun"
    show SpaceFood = "space food"
    show BathingSuit = "bathing suit"
    show Tamagotchi = "tamagotchi"


type Universe = M.Map ItemName Item

--Makes the Map from a list of items
mkUniverse :: [Item] -> Universe
mkUniverse items = M.fromList (foldr helper [] items)
    where
        helper Item { iname, weight } acc = 
            (iname, Item {iname = iname, weight = weight}):acc

broadsword :: Item
broadsword = Item Broadsword 75
jesterOutfit :: Item
jesterOutfit = Item JesterOutfit 15
bathingTub :: Item
bathingTub = Item BathingTub 30
bucket :: Item
bucket = Item Bucket 10
pulsingOrb :: Item
pulsingOrb = Item PulsingOrb 35
wand :: Item
wand = Item Wand 15
shield :: Item
shield = Item Shield 90
prophecyScroll :: Item
prophecyScroll = Item ProphecyScroll 5
potion :: Item
potion = Item Potion 20
universalTranslator :: Item
universalTranslator = Item UniversalTranslator 35
rayGun :: Item
rayGun = Item RayGun 120
spaceFood ::  Item
spaceFood = Item SpaceFood 40
bathingSuit :: Item
bathingSuit = Item BathingSuit 35
tamagotchi ::  Item
tamagotchi = Item Tamagotchi 20

--Universe of items for Level One
univOne :: Universe
univOne = mkUniverse [broadsword, jesterOutfit, bathingTub, bucket, pulsingOrb, 
                        wand, shield, prophecyScroll, potion
                     ]

--List of Item names used in Examples
itemNamesOne :: [ItemName]
itemNamesOne = M.keys univOne

--Universe of items for Level Two
univTwo :: Universe
univTwo = mkUniverse [universalTranslator, rayGun, 
                        spaceFood, bathingSuit, tamagotchi
                     ]

