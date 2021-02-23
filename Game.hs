--Author John Hayford
--Last edited 15/02/2021
import Base
import Data.Char

lowerString :: [Char] -> [Char]
lowerString str = [ toLower loweredString | loweredString <- str]

--Chunk 1 Creating a map (30 marks)
--1) function "opposite" to compute a direction’s opposite, e.g., opposite North = South
opposite :: Direction -> Direction
opposite South = North
opposite North = South
opposite East  = West
opposite West  = East

--2) simple function to represent a lack of actions in a room, which can be used for a room’s actions field
noActions :: Item -> GameState -> Next GameState
noActions  i g =  Same $ "No futher action can be done with item: " ++   show i -- ++ description $ "No Action in this room"

--3) Define a winning room (has isWinRoom value as True)
--   which requires a key to enter, has no items or monster or further doors, 
--   and no actions (use the function you defined above)

--isWinRoom = True, requires Key, actions = noActions
winRoom :: Room
winRoom = Room "winning room" "this room is the winning room" True Nothing [] [] [] noActions

--4) Define a starting room for your player from which the winRoom can be accessed and which 
--   has a spoon as one of its items (and no actions)
startRoom :: Room
startRoom = Room  "Starting Room" "Room where the game is started. There is a room available in this room. Enjoy the game player"
 False Nothing    [(Spoon, "Spoon item can be used to attack monster")] [] [(North, winRoom)] noActions
--5) Define a room leading from the previous containing a wood troll with health 10 who holds a
--   Key. Implement an “action” function which on a Spoon input checks if the room (provided
--   by the GameState parameter) has a monster
--   First define monster
myMonster :: Monster
myMonster = WoodTroll 10 Key 

leadRoom :: startRoom -> Room
leadRoom startRoom = Room "Leading Room" "Room that leads from previous room. This room has a woodtroll who holds a spoon" 
 False Nothing [] [myMonster] [] actionAttack
        



actionAttack :: Item -> GameState -> Next GameState
actionAttack Spoon (GS p r) =
        case monsters r of
                [] -> Same "No monster to attack in this room. Use your Items wisely"
                ((WoodTroll h i) :ms) ->
                        if h <= 5 
                                then
                                let r' = r {monsters = (WoodTroll 0 i) : ms}
                                in Progress "Nice Strike you killed the monster" (GS p r')
                                else
                                let s = r {monsters = (WoodTroll 5 i) : ms}
                                in Progress "Nice Strike you killed the monster" (GS p s)
--6) Define game0 as an initial GameState pointing to your start room and with a player who
--   has an empty inventory
-- Making a Player with emply inventory
playerStart :: Player
playerStart = Player "Bright" []
    
game0 :: GameState
game0 = GS playerStart startRoom


-- Chunk 2
--7) Implement a Parsable instance for Item. Item names should be parsed lower case.
-- class Parsable t where
--   parse :: String -> Maybe t
instance Parsable Item where
    parse "spoon" =  Just Spoon
    parse "key"   = Just Key
    parse _       = Nothing
--8) Implement a Parsable instance for Direction. Directions should be parsed lower case
instance Parsable Direction where
        parse "north" = Just North
        parse "south" = Just South
        parse "east"  = Just East
        parse "west"  = Just West
        parse _       = Nothing 

--9) Implement a Parsable instance for Command which parses the following inputs to commands:
instance Parsable Command where
        parse "go north" = Just (Move North)
        parse "go south" = Just (Move South)
        parse "go east"  = Just (Move East)
        parse "go west"  = Just (Move West)
        
        parse "grab spoon" = Just (PickUp Spoon)
        parse "grab key"   = Just (PickUp Key)

        parse "use spoon"  = Just (Use Spoon)
        parse "use key"    = Just (Use Spoon)
        parse "end"        = Just End
        parse _            = Nothing
--10) Define a function tellResponse that takes a string and outputs it (i.e., writes to standard (3 marks)
--   output) with the following form, e.g., where message is the input string here:
-- tellContextLine :: String -> IO ()
-- tellContextLine s = putStrLn $ "   " ++ s ++ "."
tellResponse :: String -> IO ()
tellResponse s = putStrLn $ " < " ++ s ++ " ."
--11) Define a function readCommand :: IO (Maybe Command) that outputs the string "> " 
--using putStr2 and then reads a line of input from the user with getLine and returns the result
--of parsing the user’s input string via parse

