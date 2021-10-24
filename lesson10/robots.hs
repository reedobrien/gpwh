-- listing 10.8.A A `robot` constructor
robot (name,attack,hp) = \message -> message (name,attack,hp)

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

p bot = printRobot bot

--  listing 10.13
damage bot attackDamage = bot (\(n,a,h) ->
                          robot (n,a,h-attackDamage))
-- listing 10.14
fight bot defender = damage defender attack
    where attack = if getHP bot > 10
                   then getAttack bot
                   else 0

-- Extending the exercise
-- Use map on a list of robot objects to get the life of each robot in the
-- list.
getLife robots = map getHP robots

-- Write a threeRoundFight function that takes two robots and has them fight
-- for three rounds, returning the winner. To avoid having so many different
-- variables for robot state, use a series of nested lambda functions so you
-- can just overwrite robotA and robotB.
a = robot ("a", 10, 100)
b = robot ("b", 20, 50)
c = robot ("c", 30, 60)
d = robot ("d", 20, 50)

threeRoundFight = (\a b -> fight a b) a b
-- threeRoundFight a b =
--     (\a b ->
--      (\a b ->
--       (\a b ->
--        (\a b ->
--         (\a b ->
--          (\a b -> fight b a)
--           a (fight a b))
--         (fight b a) b)
--        a (fight a b))
--       (fight b a) b)
--      a (fight a b)
--     ) a b

-- test  = threeRoundFight a b

{-
 -
w = (\a b -> fight a b)  a b
p w
"b attack:20 hp:40"

-}

-- threeRoundFight (na,aa,ha) (nb,ab,hb) =
--   (\(na,aa,ha) (nb,ab,hb) ->
--    (\(na,aa,ha) (nb,ab,hb) ->
--    (\(na,aa,ha) (nb,ab,hb) ->
--     (\(na,aa,ha) (nb,ab,hb) ->
--     (\(na,aa,ha) (nb,ab,hb) ->
--      (\(na,aa,ha) (nb,ab,hb) -> fight (nb,ab,hb) (na,aa,ha))
--      (na,aa,ha) (fight (na,aa,ha) (nb,ab,hb)))
--     (fight (nb,ab,hb) (na,aa,ha) ) (nb,ab,hb))
--     (na,aa,ha) (fight (na,aa,ha) (nb,ab,hb)))
--    (fight (nb,ab,hb) (na,aa,ha) ) (nb,ab,hb))
--    (na,aa,ha) (fight (na,aa,ha) (nb,ab,hb))
--   ) (na,aa,ha) (nb,ab,hb)



-- Create a list of three robots. Then create a fourth robot. Use partial
-- application to create a closure for the fight method so the fourth robot can
-- fight all three robots at once, using map. Finally, use map to get the
-- remaining life from the rest of the robots.

