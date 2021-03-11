import Base
-- getting access to the list of monster
monstersList :: GameState -> [Monster]
monstersList gamestate = monsters (room gamestate)


-- deadMonster WoodTroll  (health :: Int, holding :: Item)

--checking number of Monsters
monsterNum :: [Monster] -> Int
monsterNum []     = 0 --base case
monsterNum (_:xs) = 1 + monsterNum xs

-- checking is there is monster in List
isNoMonster :: [Monster] -> Bool
isNoMonster i  
        |  monsterNum i == 0   = True
        |  otherwise           = False 

-- monsHealth ((WoodTroll health  _))
--         | health <= 5 = health -5
--         | health > 5 = health -5

-- actionAttack :: Item -> GameState -> Next GameState
-- actionAttack i (GS _ (Room _ _ _ _ _ monsterList _ _)) 
--         | i == Spoon && isNoMonster monsterList == True  = Same "No monster to attack in this room. Use your Items wisely"
