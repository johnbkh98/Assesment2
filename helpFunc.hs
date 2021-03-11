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

--17) bonus -- helperFunction
removeFrom :: Item -> [Item] -> [Item]
removeFrom _ [] = []
removeFrom i (x:xs)
        |i==x  = removeFrom i xs
        |otherwise = x: removeFrom i xs