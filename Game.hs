--Author John Hayford
--Last edited 15/02/2021
import Base
import Data.Char

lowerString :: [Char] -> [Char]
lowerString str = [ toLower loweredString | loweredString <- str]

-- *****Chunk 1*****
--1)
opposite :: Direction -> Direction
opposite South = North
opposite North = South
opposite East  = West
opposite West  = East

--2) 
noActions :: Item -> GameState -> Next GameState
noActions  i g =  Same $ "No futher action can be done with item: " ++   show i -- ++ description $ "No Action in this room"

--3)
winRoom :: Room
winRoom = Room "winning room" "this room is the winning room" True Nothing [] [] [] noActions

--4)
startRoom :: Room
startRoom = Room  "Starting Room" "Room where the game is started. There is a room available in this room. Enjoy the game player"
 False Nothing    [(Spoon, "Spoon item can be used to attack monster")] [] [(North, winRoom)] noActions

--5)
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
--6) 
playerStart :: Player
playerStart = Player "Bright" []
    
game0 :: GameState
game0 = GS playerStart startRoom


-- ******Chunk 2******
--7)
-- class Parsable t where
--   parse :: String -> Maybe t
instance Parsable Item where
    parse "spoon" =  Just Spoon
    parse "key"   = Just Key
    parse _       = Nothing
--8)
instance Parsable Direction where
        parse "north" = Just North
        parse "south" = Just South
        parse "east"  = Just East
        parse "west"  = Just West
        parse _       = Nothing 

--9)
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
        
--10) 
tellResponse :: String -> IO ()
tellResponse s = putStrLn $ " < " ++ s ++ " ."

--11)
readCommand :: IO (Maybe Command)
readCommand = do 
        putStr "> "
        i <- getLine 
        putStrLn $ read i
        return(parse i)

-- *****Chunk 3****
--12)
deleteFrom :: Eq a => a -> [(a, b)] -> [(a, b)]
deleteFrom a [] = []
deleteFrom a ((l, r) :xs) | a==l      = deleteFrom a xs
                          |  otherwise = (l, r): deleteFrom a xs



--13)
leaveRoom :: Room -> Direction -> Room -> Room
leaveRoom = undefined 
--14)
step :: Command -> GameState -> Next GameState
step c g = undefined 