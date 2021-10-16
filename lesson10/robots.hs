-- listing 10.8.A A `robot` constructor
robot (name, attack, hp) = \message -> message (name, attack, hp)

--  listing 10.9
name (n,_,_)   = n
attack (_,a,_) = a
hp (_,_,hp)    = hp


-- listing 10.10
getName bot   = bot name
getAttack bot = bot attack
getHP bot     = bot hp

-- listing 10.11
setName bot newName     = bot (\(n,a,h) -> robot (newName,a,h))
setAttack bot newAttack = bot (\(n,a,h) -> robot (n,newAttack,h))
setHP bot newHP         = bot (\(n,a,h) -> robot (n,a,newHP))

-- listing 10.12
printRobot bot = bot (\(n,a,h) -> n ++
                                  " attack:" ++ (show a) ++
                                  " hp:" ++ (show h))
