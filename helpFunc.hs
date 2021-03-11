module HelpFunc where
import Base

--5)-- monster for leadRoom with 10hp nad a Key
myMonster :: Monster
myMonster = WoodTroll 10 Key

--14) helperFunctions
checkList :: Eq a => a -> [(a,b)] -> Bool
checkList _ []           = False 
checkList x ((y,z) : xs) 
        | x==y      = True 
        | otherwise = checkList x xs


getRoom :: Direction -> [(Direction, Room)] -> Room 
getRoom x ((y,z) : xs)
        | x==y  = z
        | otherwise = getRoom x xs

--17) bonus
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