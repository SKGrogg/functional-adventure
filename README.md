# functional-adventure
A simple text-based computer game written entirely in Haskell. The current gameplay is simple and unstructured, but the underlying mechanics are in place.

# Context: 

This was created for a Functional Programming Final Project at the University of Chicago. Much of the functionality was required per the assignment guidelines, but there was room for cuustomization and implementation of additional feature(s), which are ouutlined below.

# To Run:

Once inside the proper directory, run the code "stack exec functional-adventure-exe" from the command line to start the game.

# Acknowledgements: 

I would like to whole heartedly thank Bonnie Marcus and Elizabeth Ng for their help and support this semester. They took to functional a bit quicker than I did, and helped explain certain concepts and prompts to me in a way that was incredibly clear and helpful in completing this project.

# How to Play: 

The premise of the game is simple and goofy. You start a medieval peasant trapped in a castle. If you take the pulsing orb from the dungeon (North x2 from starting position) up to the tower (South x3 from dungeon), you complete level one and be brought to level two. In the lore of the game, the pulsing orb is a homing beacon for the ship, which whisks you away.

To win the game, the player must bring the bathing suit from the engine room (East of starting position) to the room with the infinity pool (West x2 from engine room). Please remember, this is a peasant from the dark ages, so what more of a win could one such as them accomplish than kicking back, taking a dip, and watching the comsos zip by.

# New Feature: 

I added a very simple new feature to my game that allows a user to view an item's weight that's currently in their inventory. The intent here was to help the user decide which items to drop if they are carrying too much. The user can see this feature by typing "view blank", where blank is a valid item in their inventory.

# RunTime Bug: 

The only bug I ran into is the prompt arrow "->" kept appearing after the users' input (rather than before) when repl was run from main. To make the game a bit more clear, I ended up putting that "->" on a line of its own, which is not what the descripton called for, but was more clear than the bug I kept running into.
