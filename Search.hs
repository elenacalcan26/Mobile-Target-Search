{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Nil | Node { state :: s,
                             action :: Maybe a,
                             parentNode :: Node s a,   
                             depth :: Int,
                             cost :: Float,
                             succs :: [Node s a] } deriving (Show)

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    node1 == node2 = (state node1) == (state node2)

instance Ord s => Ord (Node s a) where
    node1 <= node2 = (state node1) <= (state node2)


{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState node = (state node)

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent Nil = Nothing
nodeParent node = Just $ parentNode node  

nodeDepth :: Node s a -> Int
nodeDepth Nil = 0
nodeDepth node = depth node

nodeChildren :: Node s a -> [Node s a]
nodeChildren Nil = []
nodeChildren node = succs node

nodeHeuristic :: Node s a -> Float
nodeHeuristic Nil = 0.0
nodeHeuristic node = cost node

nodeAction :: Node s a -> Maybe a
nodeAction Nil = Nothing
nodeAction node = action node

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = node where 
    node = Node initialState Nothing Nil 0 (h initialState) newSuccs -- se initializeaza nodul curent
    -- pentru fiecare succesor a starii curente se apeleaza functia de helper pentru crearea nodurilor si pentru construirea arborelui
    newSuccs = map (\(st, act) -> createStateSpaceHelper act st node 1 (h act)) (successors initialState)

{-
    Functie ajutatoare pentru generarea succesorilor starii curente. Primeste ca parametru starea curenta, actiunea nodul parinte, 
    nivelul pe care se afla starea curenta si costul pana la starea finala
-}
createStateSpaceHelper :: (ProblemState s a) => s -> a -> Node s a -> Int -> Float -> Node s a
createStateSpaceHelper s a parent currDepth nodeCost = node where
    node = Node s (Just a) parent currDepth nodeCost newSuccs -- se initializeaza nodul
    -- se parcurg succcesorii starii curente si se apeleaza functia pentru a construi arborele
    newSuccs = map (\(st, act) -> createStateSpaceHelper act st node (currDepth + 1) (h act) ) (successors s)

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
-- se parcurg succesorii  si se salveaza in lista nodurile care nu sunt vizitate
suitableSuccs node visited = foldl (\acc n -> if (S.member (nodeState n) visited) == True then acc else acc ++ [n] ) [] (nodeChildren node)

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = 
    let nodePrio = ((nodeHeuristic node) + (fromIntegral (nodeDepth node) :: Float)) -- se calculeaza prioritatea pentru nodul care se insereaza in coada
    -- se insereaza nodul in coada de prioritati; daca exista elementul deja inserat, se insereaza cea cu prioritate minima
    in PQ.insertWith (\x y -> if x < y then x else y) node nodePrio frontier 

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
-- se parcurg nodurile succesor nevizitate si se adauga in coada
insertSuccs node frontier visited = foldl (\acc e -> insertSucc acc e) frontier (suitableSuccs node visited)

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier = 
        let res = deleteFindMin frontier -- extrag nodul din frontiera
        -- daca nodul extras din frontiera este cel final, se returneaza; altfel nodul este marcat ca vizitat si se insereaza succesorii sai in coada de prioritati
        in if (isGoal $ state $ fst res) == True then fst res 
            else astar' (S.insert (state $ fst res) visited) (insertSuccs (fst res) (snd res) (S.insert (state $ fst res) visited))

{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' S.empty (insertSucc PQ.empty initialNode) -- se apeleaza astar' dandu-se ca parametri o multime goala si o coada ce contine nodul initial

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

extractPath :: Node s a -> [(a, s)]
extractPath goalNode = 
    let parents = getParents goalNode [] -- se ia o lista ce contine fiecare parinte de la nodul initial pana la nodul final
        path = map (\n -> (fromJust (action n), state n)) parents -- se creeaza calea cu lista de perechi (actiune, stare)
    in reverse path -- se inverseaza lista 

{-
    extrage parintii fiecarui nod, incepand de la un nod dat ca parametru
-}
getParents :: Node s a -> [Node s a] -> [Node s a]
getParents node list = if depth node == 0 then list else getParents (parentNode node) (list ++ [node])