module Game where
--Author John Hayford
--Last edited 11/03/2021
import Base
import Data.Char
import HelpFunc

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
noActions  i g =  Same $ "No futher action can be done with item: " ++   show i 

--3)
winRoom :: Room
winRoom = Room "winning room" "Congratulations you made it to the winning Room" True (Just Key) 
        [] [] [(West,startRoom)] noActions

--4)
startRoom :: Room
startRoom = Room  "Kicthen" "breezy, cold and smells delightful"
 False Nothing [(Spoon, "In a mug, on the table,"), (Sandwich,"On the table,")] [] 
 [(South, leadRoom),(East, winRoom)] noActions

--5) monster defined in HelpFunc.hs
actionAttack :: Item -> GameState -> Next GameState
actionAttack Spoon (GS p r) =
 case monsters r of
        [] -> Same "No monster to attack in this room. Use your Items wisely"
        ((WoodTroll h i) :ms) ->
                if h <= 5 
                        then
                        let r' = r {monsters = [], items = (Key, "on the floor"): items r}
                        in Progress "Nice Strike you killed the monster" (GS p r')
                        else
                        let s = r {monsters = WoodTroll 5 i : ms}
                        in Progress "Monster took -5hp from that attack and is still standing" 
                        (GS p s)

-- then Progress "you hit the monster it looks weak" (GS p r {monseters = [WoodTroll 5 [Key]})

                                

--definning leadRoom
leadRoom :: Room
leadRoom = Room "Corridor" "quite dark but you see muddy foot prints" 
 False Nothing [] [myMonster] [(North, startRoom)] actionAttack



--6/ 17) 
playerStart :: Player
playerStart = Player "Bright" [] 20 --bonus added health
    
game0 :: GameState
game0 = GS playerStart startRoom


-- ******Chunk 2******
--7)

instance Parsable Item where
    parse "spoon"  =  Just Spoon
    parse "key"    = Just Key
    parse "sandwich" = Just Sandwich
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

        parse ('g':'o':' ':dir)  =
                case parse dir of 
                        Just x -> Just (Move x)
                        Nothing  -> Nothing 

        parse ('g':'r':'a':'b':' ':item) =
                case parse item of
                        Just x -> Just (PickUp x)
                        Nothing -> Nothing 
        
        parse ('u':'s':'e':' ':item) =
                case parse item of
                        Just x -> Just (Use x)
                        Nothing -> Nothing
        parse "end" = Just End

--10) 
tellResponse :: String -> IO ()
tellResponse s = putStrLn $ " < " ++ s ++ ". "

--11)
readCommand :: IO (Maybe Command)
readCommand = do 
        putStr "> "
        cmd <- getLine 
        let inp = parse cmd
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

-- --14 /17) getRoom and checkList in HelpFunc.hs
--implementing step
step :: Command -> GameState -> Next GameState
step (Move dirc) (GS p r) 
        |checkList dirc (doors r) = 
                case requires (getRoom dirc (doors r)) of
                        Nothing -> Progress ("You go through the " ++ show dirc ++ " door"  )
                                        (GS p (leaveRoom r dirc (getRoom dirc (doors r))))
                        Just i  -> if i `elem` inventory p
                                then Progress ("You've unlocked the door and go " ++ show dirc )
                                        (GS p (leaveRoom r dirc (getRoom dirc (doors r))))
                                else Same ("Door locked, you dont have the " ++ show i)
        | otherwise = Same ("There is no door to " ++ show dirc)

step (PickUp item) (GS p r) =
        case lookup item (items r) of
                Nothing -> Same (show item ++ " is not in the room")
                Just i -> Progress  (show item ++ " has been added to your inventory")
                        (GS p {inventory = item : inventory p} r 
                                {items = deleteFrom item (items r)}) 
        
step (Use item) (GS p r) 
        | item `elem` inventory p = 
                case item of
                        Spoon -> actions r item (GS p r)
                        Sandwich -> Progress ("You ate the Sandwich, your health is now " ++ 
                                        show (pHealth p + 50))
                                         (GS p {pHealth = pHealth p + 50, 
                                         inventory = removeFrom item (inventory p)} r)
        | otherwise = Same (show item ++ " is not in your inventory")


play :: GameState -> IO()
play gs = do
                tellContext gs
                playLoop gs
 

playLoop :: GameState -> IO ()
playLoop (GS p r) =
        if isWinRoom r then
                do 
                        tellResponse ("You beat the game Good job " ++ show (playerName p))
                        return ()
        else 
                do
                        x <- readCommand
                        case x of
                                Nothing -> do
                                        tellResponse "Unknown command. Play the game properly!"
                                        playLoop (GS p r)
                                
                                (Just End) -> do
                                        tellResponse "You quit the game for no apparent reason!"
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

--17) Bonus *Added item Sandwich that give health to player*

