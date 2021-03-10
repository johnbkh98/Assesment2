module Game where
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
winRoom = Room "winning room" "this room is the winning room you won" True (Just Key) [] [] [(West,startRoom)] noActions

--4)
startRoom :: Room
startRoom = Room  "Starting Room" "Room where the game is started. There is a room available in this room. Enjoy the game player"
 False Nothing [(Spoon, "Spoon item can be used to attack monster")] [] [(South, leadRoom),(East,winRoom)] noActions

--5)
-- monster for leadRoom with 10hp nad a Key
myMonster :: Monster
myMonster = WoodTroll 10 Key 
--action implementation. Spoon can only deal 5 dmg if there is a montser in the room
-- Nothing will happen if you attack with spoon with no monster in room
-- if there is a montser, the monster loses 5hp, if its hp is > 5 it dies.
actionAttack :: Item -> GameState -> Next GameState
actionAttack Spoon (GS p r) =
        case monsters r of
                [] -> Same "No monster to attack in this room. Use your Items wisely"
                ((WoodTroll h i) :ms) ->
                        if h <= 5 
                                then
                                let r' = r {monsters = WoodTroll 0 i : ms}
                                in Progress "Nice Strike you killed the monster" (GS p r')
                                else
                                let s = r {monsters = WoodTroll 5 i : ms}
                                in Progress "Monster took -5hp from that attack and is still standing" (GS p s)

                                -- then Progress "you hit the monster it looks weak" (GS p r {monseters = [WoodTroll 5 [Key]})

                                -- then Progress "you hit the monster it looks weak" (GS p r{monsters r = [WoodTroll (health(head(monsters r))-5) [Key]})



--definning leadRoom
leadRoom :: Room
leadRoom = Room "Leading Room" "Room that leads from previous room. This room has a woodtroll who holds a spoon" 
 False Nothing [] [myMonster] [(North, startRoom)] actionAttack

--6) 
playerStart :: Player
playerStart = Player "Bright" []
    
game0 :: GameState
game0 = GS playerStart startRoom


-- ******Chunk 2******
--7)

instance Parsable Item where
    parse "spoon"  =  Just Spoon
    parse "key"    = Just Key
    parse "gloves" = Just Gloves
    parse _        = Nothing
--8)
instance Parsable Direction where
        parse "north" = Just North
        parse "south" = Just South
        parse "east"  = Just East
        parse "west"  = Just West
        parse _       = Nothing 

--9)
instance Parsable Command where
        
        --First implementation
        

        --Second implementation negates the need to change command if someone addes new direction
        -- Following Dominic's advice
        parse ('g':'o':' ':dir)  =
                case parse dir of 
                        Just x -> Just (Move x)
                        Nothing  -> Nothing 

        parse ('g':'r':'a':'b':item) =
                case parse item of
                        Just x -> Just (PickUp x)
                        Nothing -> Nothing 
        
        parse ('u':'s':'e':item) =
                case parse item of
                        Just x -> Just (Use x)
                        Nothing -> Nothing
        parse "end" = Just End

        


--10) 
tellResponse :: String -> IO ()
tellResponse s = putStrLn $ " < " ++ s ++ " ."

--11)
readCommand :: IO (Maybe Command)
readCommand = do 
        putStr "> "
        i <- getLine 
        let inp = parse i
        return inp

-- *****Chunk 3****
--12)
deleteFrom :: Eq a => a -> [(a, b)] -> [(a, b)]
deleteFrom a [] = []
deleteFrom a ((l, r) :xs) | a==l       = deleteFrom a xs
                          |  otherwise = (l, r): deleteFrom a xs

--13)

leaveRoom :: Room -> Direction -> Room -> Room
leaveRoom fR dir tR = tR {doors = tR'} where 
         back = opposite dir
         tR'  = (back, fR) : deleteFrom back (doors tR)




-- --14)
-- actionAttack :: Item -> GameState -> Next GameState
-- actionAttack Spoon (GS p r) =

checkList :: Eq a => a -> [(a,b)] -> Bool
checkList _ []           = False 
checkList x ((y,z) : xs) 
        | x==y      = True 
        | otherwise = checkList x xs

getRoom :: Direction -> [(Direction, Room)] -> Room 
getRoom x ((y,z) : xs)
        | x==y  = z
        | otherwise = getRoom x xs
        
--implementing step
step :: Command -> GameState -> Next GameState
step (Move dirc) (GS p r) 
        |checkList dirc (doors r) = 
                case requires (getRoom dirc (doors r)) of
                        Nothing -> Progress ("You go through the " ++ show dirc ++ " door"  )
                                        (GS p (leaveRoom r dirc (getRoom dirc (doors r))))
                        Just i -> if i `elem` inventory p
                                then Progress ("You've unlocked the door and go " ++ show dirc )
                                        (GS p (leaveRoom r dirc (getRoom dirc (doors r))))
                                else Same ("Door locked, you dont have the " ++ show i)
        | otherwise = Same ("There is no door to" ++ show dirc)
        
-- step (PickUp item) (GS p r) 
--         |checkList item (items r) =
--                 case items r of
--                         Just i -> if i `elem` inventory p
--                                 then Progress ("You've picked up " ++ show item)
--                                         (GS p (deleteFrom r item (getRoom item (items r))))
--                                 else Same ("item unavaible for pick up")
        
--         |otherwise                = Same (show item ++ " is not in this room")


step (Use item) (GS p r) = undefined


--15)

play :: GameState -> IO()
play gs = do
                tellContext gs
                playLoop gs
 

playLoop :: GameState -> IO ()
playLoop (GS p r) =
        if isWinRoom r then
                do 
                        tellResponse "You Managed to get to the Win Room!! Congrats Player"
                        return ()
        else 
                do
                        x <- readCommand
                        case x of
                                Nothing -> do
                                        tellResponse "Wrong Command Entered. Play the game properly!"
                                        playLoop (GS p r)
                                
                                (Just End) -> do
                                        tellResponse "Game Over you quit the game for no apparent reason!"
                                        return()
                                
                                Just cmd -> do
                                        case step cmd (GS p r) of
                                                (Progress str nxtG) -> do
                                                        tellResponse str 
                                                        tellContext nxtG
                                                        playLoop nxtG
                                                
                                                (Same str') -> do
                                                        tellResponse str'
                                                        tellContext (GS p r)
                                                        playLoop (GS p r)

--16)
main :: IO ()
main = play game0

--17) Bonus item gloves.
actionGloves :: Item -> GameState -> Next GameState
actionGloves Gloves (GS p r) =
        case monsters r of
                [] -> Same "No monster to attack in this room. Use your Items wisely"
                ((WoodTroll h i) :ms) ->
                        if h <= 5 
                                then
                                let r' = r {monsters = WoodTroll 0 i : ms}
                                in Progress "Nice punch, you killed the monster!" (GS p r')
                                else
                                let s = r {monsters = WoodTroll 5 i : ms}
                                in Progress "The Monster ate that punch up but took -5hp and is still standing" (GS p s)