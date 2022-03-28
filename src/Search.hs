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

data Node s a = Node {
    gameState :: s,
    action :: Maybe a,
    parent :: Maybe (Node s a),
    depth :: Int,
    heuristic :: Float,
    children :: [(Node s a)]
}

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    (Node state1 _ _ _ _ _) == (Node state2 _ _ _ _ _) = state1 == state2

instance Ord s => Ord (Node s a) where
    (Node state1 _ _ _ _ _) <= (Node state2 _ _ _ _ _) = state1 <= state2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState (Node stateNode _ _ _ _ _) = stateNode

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node _ _ parentNode _ _ _) = parentNode
-- nodeParent = undefined

nodeDepth :: Node s a -> Int
nodeDepth (Node  _ _ _ depthNode _ _) = depthNode

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node _ _ _ _ _ childrenNode) = childrenNode

nodeHeuristic :: Node s a -> Float
nodeHeuristic (Node _ _ _ _ heuristicNode _) = heuristicNode

nodeAction :: Node s a -> Maybe a
nodeAction (Node _ actionNode _ _ _ _) = actionNode
-- nodeAction = undefined

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

createNodeFromState :: (ProblemState s a, Eq s) => Node s a -> (a, s) -> Node s a
createNodeFromState parentNode pair = newChild
    where
        newChild = (Node newState (Just newAction) (Just newParent) newDepth newHeuristic newChildren)
        newState = snd pair
        newAction = fst pair
        newParent = parentNode
        newDepth = (nodeDepth parentNode) + 1
        newHeuristic = h (snd pair)
        newChildren = createChildrenNodes newChild (successors (snd pair))

createChildrenNodes :: (ProblemState s a, Eq s) => Node s a -> [(a, s)] -> [Node s a]
createChildrenNodes parentNode pairs = map (createNodeFromState parentNode) pairs

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = newNode
    where
        newNode = (Node newState newAction newParent newDepth newHeuristic newChildren)
        newState = initialState
        newAction = Nothing
        newParent = Nothing
        newDepth = 0
        newHeuristic = h initialState
        newChildren = createChildrenNodes newNode (successors initialState)

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
suitableSuccs node visited = foldl (\ acc childNode -> if (elem (gameState childNode) visited) == True then acc else acc ++ [childNode]) [] (children node)

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
insertSucc frontier node = PQ.insertWith min node ((nodeHeuristic node) + fromIntegral (nodeDepth node)) frontier

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = foldl insertSucc frontier (suitableSuccs node visited)

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
astar' visited frontier
 | isGoal (gameState (fst (deleteFindMin frontier))) == True = fst (deleteFindMin frontier)
 | otherwise = astar' ( S.insert (gameState (fst (deleteFindMin frontier))) visited ) ( insertSuccs (fst (deleteFindMin frontier)) (snd (deleteFindMin frontier)) ( S.insert (gameState (fst (deleteFindMin frontier))) visited ) ) 


{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' S.empty (PQ.insert initialNode (nodeHeuristic initialNode) PQ.empty)

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

extractPath :: Node s a -> [(a, s)]
extractPath goalNode 
 | isNothing (parent goalNode) = []
 | otherwise = extractPath (fromJust (parent goalNode)) ++ [((fromJust (action goalNode)), (gameState goalNode))]