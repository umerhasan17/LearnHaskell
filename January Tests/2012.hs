import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp x xs
  -- = head [v | (i, v) <- xs, x == i]
  = fromJust (lookup x xs)

states :: LTS -> [State]
states lts
  = nub (states' lts [])
  where
    states' :: LTS -> [State] -> [State]
    states' [] states
      = states
    states' (((s, t), id) : xs) states
      = (s : t : states) ++ states' xs states
      
transitions :: State -> LTS -> [Transition]
transitions s lts
  = [t | t <- lts, fst(fst(t)) == s]

alphabet :: LTS -> Alphabet
alphabet
  -- = nub [a | ((s, t), a) <- lts]
  = nub . map snd 

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions (Prefix id p)
  = id : actions p
actions (Choice ps)
  = concatMap actions ps
actions _
  = []

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts ts ps
  = accepts' ts (snd (head ps)) ps
  where
    accepts' :: [Id] -> Process -> [ProcessDef] -> Bool
    accepts' [] _ _
      = True
    accepts' (t : ts) (STOP) ps
      = False
    accepts' (t : ts) (Ref id) ps
      = accepts' (t : ts) p ps
      where
        p = lookUp id ps
    accepts' (t : ts) (Prefix id p) ps
      = (t == id) && accepts' ts p ps
    accepts' (t : ts) (Choice ps') ps
      = or (accepts'' (t : ts) ps' ps)
      where
        accepts'' :: [Id] -> [Process] -> [ProcessDef] -> [Bool]
        accepts'' ts [] ps
          = [True]
        accepts'' ts (p : ps') ps
          = (accepts' ts p ps) : (accepts'' ts ps' ps)

------------------------------------------------------
-- PART III

composeTransitions :: Transition -> Transition 
                  -> Alphabet -> Alphabet 
                  -> StateMap 
                  -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s, t), id1) ((s', t'), id2) a1 a2 m
  | id1 == id2 = [((i1, i4), id1)]
  | combine    = []
  | mem2       = [((i1, i2), id1)] 
  | mem1       = [((i1, i3), id2)]
  | otherwise  = [((i1, i3), id2), ((i1, i2), id1)] 
  where
    mem1    = elem id1 a2
    mem2    = elem id2 a1
    combine = mem1 &&  mem2
    i1      = lookUp (s, s') m
    i2      = lookUp (t, s') m
    i3      = lookUp (s, t') m
    i4      = lookUp (t, t') m

-- pruneTransitions :: [Transition] -> LTS
-- pruneTransitions ts
--   = nub (visit 0 [])
--     where
--       visit :: State -> [State] -> [Transition]
--       visit s ss
--         | s `elem` ss = []
--         | otherwise   = ts' ++ concatMap (flip visit (s : ss)) ss'
--         where
--           ts' = transitions s ts
--           ss' = map (snd . fst) ts'
    

pruneTransitions :: [Transition] -> LTS
pruneTransitions ts
  = nub (visit 0 [] ts)

visit :: State -> [State] -> [Transition] -> LTS
visit s xs ts
  | elem s xs = []
  | otherwise = ts' ++ visit' allTos xs' ts
  where
    ts' = transitions s ts
    (allFroms, allTos) = unzip (fst (unzip ts'))
    xs' = allFroms ++ xs
    visit' :: [State] -> [State] -> [Transition] -> LTS
    visit' [] xs ts
      = []
    visit' (y : ys) xs ts
      = (visit y xs ts) ++ (visit' ys xs ts)

------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose lts1 lts2
  = (pruneTransitions . concat) cs
  where
    a1 = alphabet lts1
    a2 = alphabet lts2
    s1 = states lts1
    s2 = states lts2
    sentinels1 = map (\s -> ((s, 0), "$1"))
    sentinels2 = map (\s -> ((s, 0), "$2"))
    ps = [(x, y) | x <- s1, y <- s2]
    -- ts = [(t1, t2) | (s1, s2) <- ps, t1 <- transitions s1 lts1, t2 <- transitions s2 lts2]
    cs = [composeTransitions t1 t2 a1 a2 (getStateMap s1 s2) | t1 <- lts1 ++ sentinels1 s1, t2 <- lts2 ++ sentinels2 s2]

getStateMap :: [State] -> [State] -> StateMap
getStateMap ss1 ss2 
  = zip [(s1, s2) | s1 <- ss1, s2 <- ss2] [0..length ss1 * length ss2 - 1]

------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS 
  = undefined

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]
