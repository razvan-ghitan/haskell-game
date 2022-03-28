{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.Maybe

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game {
    hunter :: Position,
    targets :: [Target],
    obstacles :: [Position],
    gateways :: [(Position, Position)],
    size :: (Int, Int)

    -- *** Optional *** 
  
    --Dacă aveți nevoie de o funcționalitate particulară,
    --instantiați explicit clasele Eq și Ord pentru Game.
    --În cazul acesta, eliminați deriving (Eq, Ord) din Game.
} deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
functie :: Game -> String -> Position -> String
functie game acc p
 | p == (hunter game) = acc ++ ['!']
 | elem p [ (y, z) | (y, z) <- (map position (targets game)) ] = acc ++ ['*']
 | elem p (obstacles game) == True = acc ++ ['@']
 | elem p [ (y, z) | (y, z) <- (map fst (gateways game)) ] == True = acc ++ ['#']
 | elem p [ (y, z) | (y, z) <- (map snd (gateways game)) ] == True = acc ++ ['#']
 | (fst p) /= (fst (size game)) - 1 && (snd p) == snd (size game) = acc ++ ['\n']
 | (fst p) == (fst (size game) - 1) && (snd p) == snd (size game) = acc
 | otherwise = acc ++ [' ']

--  (fst p) /= (fst (size game) - 1)

gameAsString :: Game -> String
gameAsString game = foldl ( \ acc p -> functie game acc p) "" [ (x, y) | x <- [0..(fst (size game)) - 1], y <- [0..snd (size game)] ]   

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame row collumn = newGame
    where
        newGame = Game hunter target obstacles gateways size
        hunter = (1, 1)
        target = []
        obstacles = [ (0, y) | y <- [0..collumn - 1] ] ++ [ (x, 0) | x <- [1..row - 1] ] ++ [ (row - 1, y) | y <- [1..collumn - 1] ] ++ [ (x, collumn - 1) | x <- [1..row - 1] ]
        gateways = []
        size = (row, collumn)

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter p (Game hunter targets obstacles gateways size)
    | elem p [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == False || elem p obstacles == True = Game hunter targets obstacles gateways size
    | elem p [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == True && elem p obstacles == False = newGame
        where
            newGame = Game newHunter newTargets newObstacles newGateways newSize
            newHunter = p
            newTargets = targets
            newObstacles = obstacles
            newGateways = gateways
            newSize = size  

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget behavior position game
    | elem position [ (x, y) | x <- [0..(fst (size game)) - 1], y <- [0..snd (size game)] ] == False || elem position (obstacles game) == True = game
    | elem position [ (x, y) | x <- [0..(fst (size game)) - 1], y <- [0..snd (size game)] ] == True && elem position (obstacles game) == False = newGame
        where
            newGame = Game newHunter newTargets newObstacles newGateways newSize
            newHunter = (hunter game)
            newTargets = (targets game) ++ [(Target position behavior)]
            newObstacles = (obstacles game)
            newGateways = (gateways game)
            newSize = (size game) 

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway (position1, position2) (Game hunter targets obstacles gateways size)
    | elem position1 [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == False || elem position2 [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == False = Game hunter targets obstacles gateways size
    | elem position1 [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == True && elem position2 [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == True = newGame
        where
            newGame = Game newHunter newTargets newObstacles newGateways newSize
            newHunter = hunter
            newTargets = targets
            newObstacles = obstacles
            newGateways = gateways ++ [(position1, position2)]
            newSize = size 

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle position (Game hunter targets obstacles gateways size)
    | elem position [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == False || elem position [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == False = Game hunter targets obstacles gateways size
    | elem position [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == True && elem position [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == True = newGame
        where
            newGame = Game newHunter newTargets newObstacles newGateways newSize
            newHunter = hunter
            newTargets = targets
            newObstacles = obstacles ++ [position]
            newGateways = gateways
            newSize = size 

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}

isEmptySpace :: Game -> Position -> Bool
isEmptySpace (Game hunter targets obstacles gateways size) positionS 
    | positionS == hunter = True
    | elem positionS [ (y, z) | (y, z) <- (map position targets) ] == True = True
    | elem positionS obstacles == True = False
    | elem positionS [ (y, z) | (y, z) <- (map fst gateways) ] == True = False
    | elem positionS [ (y, z) | (y, z) <- (map snd gateways) ] == True = False
    | (fst positionS) /= (fst size) - 1 && (snd positionS) == snd size = False
    | (fst positionS) == (fst size) - 1 && (snd positionS) == snd size = False
    | otherwise = True 

isGateway :: Position -> [(Position, Position)] -> Maybe Position
isGateway position gatewayss 
    | null gatewayss = Nothing
    | position == fst (head gatewayss) = Just (snd (head gatewayss))
    | position == snd (head gatewayss) = Just (fst (head gatewayss))
    | otherwise = isGateway position (tail gatewayss)

isObstacle :: Position -> [Position] -> Maybe Position
isObstacle position obstacless 
    | null obstacless = Nothing
    | position == head obstacless = Nothing
    | otherwise =  isObstacle position (tail obstacless)


attemptMove :: Position -> Game -> Maybe Position
attemptMove position (Game hunter targets obstacles gateways size)
    | elem position [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == True && 
    elem position [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == True && isGateway position gateways /= Nothing = isGateway position gateways
    | elem position [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == True && 
    elem position [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == True && isEmptySpace (Game hunter targets obstacles gateways size) position == True = Just position
    | elem position [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == True && 
    elem position [ (x, y) | x <- [0..(fst size) - 1], y <- [0..snd size] ] == True = isObstacle position obstacles
    | otherwise = Nothing

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}
general_function :: Position -> Position -> Game -> Position
general_function position_from position_to (Game hunter targets obstacles gateways size) =
    if attemptMove position_to (Game hunter targets obstacles gateways size) /= Nothing then fromJust (attemptMove position_to (Game hunter targets obstacles gateways size)) else fromJust (attemptMove position_from (Game hunter targets obstacles gateways size))   

findTargetFromBehavior :: Position -> Game -> Target
findTargetFromBehavior positionB (Game hunter targets obstacles gateways size) =
    head (foldl ( \ acc t -> if positionB == (position t) then acc ++ [t] else acc) [] targets)

goEast :: Behavior
goEast positionB@(x, y) gameB = newTarget
    where
        newTarget = Target newPosition (behavior (findTargetFromBehavior positionB gameB))
        newPosition = general_function positionB (x, y + 1) gameB 

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest positionB@(x, y) gameB = newTarget
    where
        newTarget = Target newPosition (behavior (findTargetFromBehavior positionB gameB))
        newPosition = general_function positionB (x, y - 1) gameB 

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth positionB@(x, y) gameB = newTarget
    where
        newTarget = Target newPosition (behavior (findTargetFromBehavior positionB gameB))
        newPosition = general_function positionB (x - 1, y) gameB 

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth positionB@(x, y) gameB = newTarget
    where
        newTarget = Target newPosition (behavior (findTargetFromBehavior positionB gameB))
        newPosition = general_function positionB (x + 1, y) gameB 

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
existsGateway :: Int -> Position -> [(Position, Position)] -> Bool
existsGateway valoare positionT@(xT, yT) gateways 
    | valoare == 1 && length (foldl (\ acc positionG -> if (((yT == snd (fst positionG)) || (yT == snd (snd positionG))) && ((fst (fst positionG) == xT ) || (fst (snd positionG) == xT))) then acc ++ [True] else acc) [] gateways) /= 0 = True
    | valoare == -1 && length (foldl (\ acc positionG -> if (((yT == snd (fst positionG)) || (yT == snd (snd positionG))) && ((fst (fst positionG) == xT ) || (fst (snd positionG) == xT))) then acc ++ [True] else acc) [] gateways) /= 0 = True
    | otherwise = False

bounce :: Int -> Behavior
bounce value positionB@(x, y) gameB
 | value == 1 && existsGateway value (x + 1, y) (gateways gameB)  == True = nextTarget1
 | value == 1 && existsGateway value (x + 1, y) (gateways gameB) == False && attemptMove (x + 1, y) gameB /= Nothing = nextTarget1
 | value == 1 && existsGateway value (x + 1, y) (gateways gameB) == False && attemptMove (x - 1, y) gameB /= Nothing && attemptMove (x + 1, y) gameB == Nothing = nextTarget3
 | value == -1 && existsGateway value (x - 1, y) (gateways gameB) == True  = nextTarget2
 | value == -1 && existsGateway value (x - 1, y) (gateways gameB) == False && attemptMove (x - 1, y) gameB /= Nothing = nextTarget2
 | value == -1 && existsGateway value (x - 1, y) (gateways gameB) == False && attemptMove (x + 1, y) gameB /= Nothing && attemptMove (x - 1, y) gameB == Nothing = nextTarget4

    where
        nextTarget1 = (Target positionT1 behaviorT1)
        positionT1 = fromJust (attemptMove (x + 1, y) gameB)
        behaviorT1 = bounce value
        nextTarget2 = (Target positionT2 behaviorT2)
        positionT2 = fromJust (attemptMove (x - 1, y) gameB)
        behaviorT2 = bounce value
        nextTarget3 = (Target positionT3 behaviorT3)
        positionT3 = fromJust (attemptMove (x - 1, y) gameB)
        behaviorT3 = bounce (-1)
        nextTarget4 = (Target positionT4 behaviorT4)
        positionT4 = fromJust (attemptMove (x + 1, y) gameB)
        behaviorT4 = bounce 1

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets (Game hunter targets obstacles gateways size) = newGame
    where
        newGame = (Game hunter newTargets obstacles gateways size)
        newTargets = foldl (\ acc t -> acc ++ [((behavior t) (position t) (Game hunter targets obstacles gateways size))]) [] targets 

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled position@(x, y) (Target positionT@(z, t) behaviorT)
    | (x + 1 == z) && y == t = True
    | (x - 1 == z) && y == t = True
    | (y + 1 == t) && x == z = True
    | (y - 1 == t) && x == z = True
    | otherwise = False 


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
updateGame :: Direction -> Game -> Game
updateGame direction (Game hunter@(x, y) targets obstacles gateways size) = newGame
    where
        newGame = (Game newHunter newTargets obstacles gateways size)
        newHunter
         | direction == North && (attemptMove (x - 1, y) (Game hunter targets obstacles gateways size)) /= Nothing  && isGateway (x - 1, y) gateways /= Nothing = hunterGN -- hunter (updateGame direction (Game (fromJust (attemptMove (x - 1, y) (Game hunter targets obstacles gateways size))) targets obstacles gateways size))
         | direction == South && (attemptMove (x + 1, y) (Game hunter targets obstacles gateways size)) /= Nothing && isGateway (x + 1, y) gateways /= Nothing = hunterGS -- hunter (updateGame direction (Game (fromJust (attemptMove (x - 1, y) (Game hunter targets obstacles gateways size))) targets obstacles gateways size))
         | direction == West && (attemptMove (x, y -1) (Game hunter targets obstacles gateways size)) /= Nothing && isGateway (x, y - 1) gateways /= Nothing = hunterGW -- hunter (updateGame direction (Game (fromJust (attemptMove (x - 1, y) (Game hunter targets obstacles gateways size))) targets obstacles gateways size))
         | direction == East && (attemptMove (x, y + 1) (Game hunter targets obstacles gateways size)) /= Nothing && isGateway (x, y + 1) gateways /= Nothing = hunterGE -- hunter (Game (fromJust (attemptMove (x, y + 1) (Game hunter targets obstacles gateways size))) targets obstacles gateways size)
         | direction == North && (attemptMove (x - 1, y) (Game hunter targets obstacles gateways size)) /= Nothing = fromJust (attemptMove (x - 1, y) (Game hunter targets obstacles gateways size))
         | direction == South && (attemptMove (x + 1, y) (Game hunter targets obstacles gateways size)) /= Nothing = fromJust (attemptMove (x + 1, y) (Game hunter targets obstacles gateways size))
         | direction == West && (attemptMove (x, y -1) (Game hunter targets obstacles gateways size)) /= Nothing = fromJust (attemptMove (x, y -1) (Game hunter targets obstacles gateways size))
         | direction == East && (attemptMove (x, y + 1) (Game hunter targets obstacles gateways size)) /= Nothing = fromJust (attemptMove (x, y + 1) (Game hunter targets obstacles gateways size))
         | otherwise = hunter
        
        hunterGN = fromJust (attemptMove (x - 1, y) (Game hunter targets obstacles gateways size))
        hunterGS = fromJust (attemptMove (x + 1, y) (Game hunter targets obstacles gateways size))
        hunterGW = fromJust (attemptMove (x, y -1) (Game hunter targets obstacles gateways size))
        hunterGE = fromJust (attemptMove (x, y + 1) (Game hunter targets obstacles gateways size))

        newTargetsAux = foldl (\ acc t -> if (isTargetKilled newHunter t) == False then acc ++ [((behavior t) (position t) (Game newHunter targets obstacles gateways size))] else acc ) [] targets
        newTargets = foldl (\ acc t -> if (isTargetKilled newHunter t) == False then acc ++ [t] else acc) [] newTargetsAux
        

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState direction parameter (Game hunter@(x, y) targets obstacles gateways size)
 | direction == North && parameter == True = updateGame direction (Game hunter targets obstacles gateways size)
 | direction == North && parameter == False && (attemptMove (x - 1, y) (Game hunter targets obstacles gateways size)) /= Nothing = (Game (fromJust (attemptMove (x - 1, y) (Game hunter targets obstacles gateways size))) targets obstacles gateways size)
 | direction == South && parameter == True = updateGame direction (Game hunter targets obstacles gateways size)
 | direction == South && parameter == False && (attemptMove (x + 1, y) (Game hunter targets obstacles gateways size)) /= Nothing = (Game (fromJust (attemptMove (x + 1, y) (Game hunter targets obstacles gateways size))) targets obstacles gateways size)
 | direction == West && parameter == True = updateGame direction (Game hunter targets obstacles gateways size)
 | direction == West && parameter == False && (attemptMove (x, y -1) (Game hunter targets obstacles gateways size)) /= Nothing = (Game (fromJust (attemptMove (x, y -1) (Game hunter targets obstacles gateways size))) targets obstacles gateways size)
 | direction == East && parameter == True = updateGame direction (Game hunter targets obstacles gateways size)
 | direction == East && parameter == False && (attemptMove (x, y + 1) (Game hunter targets obstacles gateways size)) /=Nothing = (Game (fromJust (attemptMove (x, y + 1) (Game hunter targets obstacles gateways size))) targets obstacles gateways size)
 | otherwise = (Game hunter targets obstacles gateways size) 
{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = if null (targets game) then False else True

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = [(North, (advanceGameState North False game)), (South, (advanceGameState South False game)), (East, (advanceGameState East False game)), (West, (advanceGameState West False game))]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal (Game hunter targets obstacles gateways size)
     | isTargetKilled hunter (head targets) == False = False
     | isTargetKilled hunter (head targets) == True = True 

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h (Game hunter targets obstacles gateways size) = hEuclidean hunter (position (head targets))

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
