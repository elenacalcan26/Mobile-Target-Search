{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
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
    target :: [Target],
    obstacle :: [Position],
    gateway :: [(Position, Position)],
    boardDimension :: Position 

} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

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

{-
    Se parcurg dimensiunile board-ului si se verifica daca pe o pozitie (x, y) se afla un hunter, target, gateway, obstacol sau este un spatiu gol  
-}

gameAsString :: Game -> String
gameAsString (Game currHunter t o g dim) = init [ if (x == fst currHunter && y == snd currHunter) then '!' 
                                    else if (any ((x, y)==) $ map (\ (Target p _) -> p) t) then '*'
                                    else if [] /= filter (\e -> elem (x, y) [fst e] || elem (x, y) [snd e]) g then '#'
                                    else if (any ((x, y)==) o) then '@'
                                    else if (y == snd dim) then '\n' -- new line matrice
                                    else ' '  | x <- [0..fst dim], y <- [0..snd dim], x < fst dim]


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
emptyGame line column = (Game hun t o g dim) where
    hun = (1, 1) -- atribuire pozitie hunter initiala
    t = [] -- nu exista target-uri
    g = [] -- nu exista gateway-uri
    -- rama matricii este ocupata de obstacole, se iau toate pozitiile si se creeaza o lista de pozitii
    o = [(x, y) | x <- [0], y <- [0..column - 1]] ++ [(x, y) | x <- [1..line - 2], y <- [0, column - 1]] ++ [(x, y) | x <- [line - 1], y <- [0..column - 1]]    
    dim = (line, column) -- atribuire dimensiune board

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter pos@(line, col) (Game currHunter t o g dim) = (Game newHunter t o g dim) where
    newHunter = if (line >= fst dim || col >= snd dim) || (line < 0 || col < 0) then currHunter --daca dimensiunile nu se incadreaza in board, hunter-ul ramane pe loc
                else if (any (pos==) $ map (\ (Target p _) -> p) t) then currHunter  -- daca pozitia este ocupata de target, hunter-ul ramane pe loc
                else if ([] /= filter (\e -> pos == fst e || pos == snd e) g) then currHunter -- daca pozitia este ocupata de un gate-way, hunter-ul ramane pe loc
                else if elem pos o then currHunter -- pozitia este ocupata de un obstacol
                else pos -- hunter-ul isi schimba pozitia

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget targetBehavior pos (Game currHunter t o g dim) = (Game currHunter newTarget o g dim) where
    newTarget = t ++ [Target pos targetBehavior] -- se creeaza un target si se concateneaza la lista de target-uri

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway (pos1, pos2) (Game currHunter t o g dim) = (Game currHunter t o newGateway dim) where
    newGateway = g ++ [(pos1, pos2)] -- se oncateneaza la lista de pozitii a gateway-urilor

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos (Game currHunter t o g dim) = (Game currHunter t newObstacle g dim) where
    newObstacle = o ++ [pos] -- noua pozitie se adauga la lista de pozitii pe care se afla obastacolele

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
attemptMove :: Position -> Game -> Maybe Position
attemptMove pos game = 
    let gatewayPos = filter (\e -> pos == fst e || pos == snd e) (gateway game) -- se extrage pozitia destinatie cu perechea sa din lista de pozitii a gateway-ului
    in 
        -- se verifica pe pozitia destinatie se afla un gateway si returneaza perechea sa
        if gatewayPos /= [] && (fst (head gatewayPos)) == pos then Just $ snd $ head gatewayPos
        else if gatewayPos /= [] && (snd (head gatewayPos)) == pos then Just $ fst $ head gatewayPos
        else if (elem pos (obstacle game)) then Nothing -- pe pozitia destiantie se afla un obstacol
        else Just pos -- se poate muta
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
goEast :: Behavior
goEast pos@(line, col) game = 
   let currTarget = head $ filter (\(Target p _) -> p == pos) (target game) -- se extrage target-ul care se afla pe pozitia curenta
       gatewayPos = filter (\e -> pos == fst e || pos == snd e) (gateway game)  -- se extrage perechea de pozitii ale gateway-ului
       move = attemptMove (line, col + 1) game -- se ia miscarea catre est, daca poate sa o faca
    in 
        if move /= Nothing then (Target (fromJust move) (behavior currTarget)) -- target-ul se misca spre est
        -- target-ul se afla pe un gateway si trece prin el
        else if gatewayPos /= [] && (fst (head gatewayPos)) == pos then (Target (snd $ head gatewayPos) (behavior currTarget)) 
        else if gatewayPos /= [] && (snd (head gatewayPos)) == pos then (Target (fst $ head gatewayPos) (behavior currTarget))
        else currTarget --ramane pe loc

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest pos@(line, col) game = 
    let currTarget = head $ filter (\(Target p _) -> p == pos) (target game) -- se extrage target-ul  
        gatewayPos = filter (\e -> pos == fst e || pos == snd e) (gateway game) -- se extrage perechea de pozitii ale gateway-ului
        move = attemptMove (line, col - 1) game -- se ia miscarea catre vest, daca  poate sa o faca
    in 
        if move /= Nothing then (Target (fromJust move) (behavior currTarget)) -- target-ul se misca catre vest
        -- target-ul se afla pe un gateway si trece prin el
        else if gatewayPos /= [] && (fst (head gatewayPos)) == pos then (Target ( snd $ head gatewayPos) (behavior currTarget))
        else if gatewayPos /= [] && (snd (head gatewayPos)) == pos then (Target (fst $ head gatewayPos) (behavior currTarget))
        else currTarget -- ramane pe loc

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth pos@(line, col) game = 
    let currTarget = head $ filter (\(Target p _) -> p == pos) (target game) -- se extrage target-ul
        gatewayPos = filter (\e -> pos == fst e || pos == snd e) (gateway game) -- se extrage perechea de pozitii ale gateway-ului
        move = attemptMove (line - 1, col) game  -- se ia miscarea catre nord, daca  poate sa o faca
    in 
        if move /= Nothing then (Target (fromJust move) (behavior currTarget)) -- target-ul se poate misca spre nord
        -- target-ul se afla pe un gateway si trece prin el
        else if gatewayPos /= [] && (fst (head gatewayPos)) == pos then (Target (snd $ head gatewayPos) (behavior currTarget))
        else if gatewayPos /= [] && (snd (head gatewayPos)) == pos then (Target (fst $ head gatewayPos) (behavior currTarget))
        else currTarget -- ramane pe loc

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth pos@(line, col) game = 
    let currTarget = head $ filter (\(Target p _) -> p == pos) (target game) -- se extrage target-ul
        gatewayPos = filter (\e -> pos == fst e || pos == snd e) (gateway game) -- se extrage perechea de pozitii ale gateway-ului
        move = attemptMove (line + 1, col) game -- se ia micarea catre sud, daca poate sa o faca 
    in 
        if move /= Nothing then (Target (fromJust move) (behavior currTarget)) -- taarget-ul se misca spre sud
        -- target-ul se alfa pe un gateway si trece prin el
        else if gatewayPos /= [] && (fst (head gatewayPos)) == pos then (Target (snd $ head gatewayPos) (behavior currTarget)) 
        else if gatewayPos /= [] && (snd (head gatewayPos)) == pos then (Target (fst $ head gatewayPos) (behavior currTarget))
        else currTarget -- ramane pe loc
    
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
bounce :: Int -> Behavior
bounce offset = if offset == 1 then bounceSouth else bounceNorth

bounceSouth :: Behavior
bounceSouth pos@(line, col) game = 
    
    let move = attemptMove (line + 1, col) game -- target-ul se misca spre sud, daca poate
        currTarget = head $ filter (\(Target p _) -> p == pos) (target game) -- se extrage target-ul
    in 
        if move == Nothing then Target (fromMaybe (-1,-1) (attemptMove (line - 1, col) game)) bounceNorth -- nu se mai poate misca si isi schimba directia 
       else Target (fromJust move) (behavior currTarget) -- se misca spre aceeasi directie

bounceNorth :: Behavior
bounceNorth pos@(line, col) game = 
    
    let move = attemptMove (line - 1, col) game -- target-ul se misca spre nord
        currTarget = head $ filter (\(Target p _) -> p == pos) (target game) -- se extrage target-ul
    in 
        if move == Nothing then Target (fromMaybe (-1,-1) (attemptMove (line + 1, col) game)) bounceSouth -- nu se mai poate misca spre nord si isi schimba directia de mers
       else Target (fromJust move) (behavior currTarget) -- se misca spre aceeasi directie
    

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets game@(Game currHunter t o g dim) = (Game currHunter updatedTargets o g dim) where
    updatedTargets = map (\(Target pos targetBehavior) -> targetBehavior pos game) t    -- se aplica behavior-ul fieacrui target

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
isTargetKilled (line, col) (Target posTarget _) = 
    if line == fst posTarget && col + 1 == snd posTarget then True
    else if line == fst posTarget && col - 1 == snd posTarget then True
    else if line + 1 == fst posTarget && col == snd posTarget then True
    else if line - 1 == fst posTarget && col == snd posTarget then True
    else False


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
advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir bool game@(Game currHunter t o g dim) = (Game newHunter remainedTargets o g dim) where
    newHunter = case dir of  -- se verifica daca hunter-ul se poate misca spre directia data ca parametru
        North -> if attemptMove (fst currHunter - 1, snd currHunter) game /= Nothing then (fromJust $ attemptMove (fst currHunter - 1, snd currHunter) game) else currHunter
        South -> if attemptMove (fst currHunter + 1, snd currHunter) game /= Nothing then (fromJust $ attemptMove (fst currHunter + 1, snd currHunter) game) else currHunter 
        East -> if attemptMove (fst currHunter, snd currHunter + 1) game /= Nothing then (fromJust $ attemptMove (fst currHunter, snd currHunter + 1) game) else currHunter 
        West -> if attemptMove (fst currHunter, snd currHunter - 1) game /= Nothing then (fromJust $ attemptMove (fst currHunter, snd currHunter - 1) game) else currHunter
    remainedTargets = 
        let safeTargets = getSafeTargets newHunter t -- se extrag target-urile care nu sunt omarate de hunter
        -- daca bool este fals, se intorc target-urile nemodificate, altfel target-urile neomorate se misca si se elimina cele care vor fi omorate de hunter 
        in if bool == False then t else (getSafeTargets newHunter (map (\(Target pos b) -> b pos game) safeTargets))


 {- 
    Returneaza target-urile care nu sunt omorate de hunter 
 -}
getSafeTargets :: Position -> [Target] -> [Target]
getSafeTargets posHunter t = filter (\x -> (isTargetKilled posHunter x) == False) t 

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = (target game) /= []


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = if game == emptyGame (fst $ boardDimension game) (snd $ boardDimension game) then [] 
                        else [(x, (advanceGameState x False game)) | x <- [North, South, East, West]]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal game = 
        let remainedTargets = [ x | x <- (target game), isTargetKilled (hunter game) x == False] -- se extrag target-urile care nu sunt omorate de hunter
        in not (remainedTargets == (target game)) -- se verifica taregt-urile initiale cu cele dupa anihilare

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game@(Game currHunter t _ _ _) = if isGoal game == True -- se verifica daca hunter-ul omoara targeturi
        then let killedTarget = head $ [x | x <- t, isTargetKilled currHunter x == True] --  se extrag target-urile omorate de hunter
             in hEuclidean currHunter (position killedTarget)  -- se calculeaza distanta euclidiana
        -- altfel se ia distanta euclidiana catre cel mai apropiat target
        -- se parcurg target-urile si se afla distanta euclidiana minima    
        else foldl (\acc x -> if (hEuclidean currHunter (position x)) > acc then acc else hEuclidean currHunter (position x)) (hEuclidean currHunter (position (head t) )) (tail t)

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

