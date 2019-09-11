import Control.Monad.State

data Tree a = EmptyTree | Tree {root :: a, left :: Tree a, right :: Tree a}

singleton :: a -> Tree a
singleton x = Tree x EmptyTree EmptyTree

findInTree :: (Eq a) => a -> Tree a -> Bool
findInTree _ EmptyTree = False
findInTree x tree = (x == (root tree)) || (findInTree x (left tree)) || (findInTree x (right tree))

uniqueNumAux :: (Tree Int,Int) -> (Tree Int,Int)
uniqueNumAux (EmptyTree,curr) = (EmptyTree,curr)
uniqueNumAux (tree,curr) = (Tree next renumTreeLeft renumRightTree, last)
 where 
 next = curr + 1
 aux1 = uniqueNumAux (left tree, next)
 aux2 = uniqueNumAux (right tree, snd $ aux1)
 last = snd aux2
 renumTreeLeft = fst $ aux1
 renumRightTree = fst $ aux2

uniqueNum :: Tree Int -> Tree Int
uniqueNum t = fst $ uniqueNumAux (t,0)

listFromTree :: Tree a -> [a]
listFromTree EmptyTree = []
listFromTree tree = (root tree) : (listFromTree $ left tree) ++ (listFromTree $ right tree)


data Turn = R | L deriving (Eq,Ord,Show)
type Path = [Turn]
type TreeState a = (Tree a, Path, Int)

treeFromPath :: Tree a -> Path -> Tree a
treeFromPath t [] = t
treeFromPath t (rol:p) = if (rol == R) then treeFromPath (right t) else treeFromPath (left t)

numeraRaiz :: State (Tree (a,Int), Int) (Tree (a,Int))
numeraRaiz = state numRaiz
 where
 numPair (p1,_) n = (p1,n)
 numRaiz (EmptyTree,n) = (EmptyTree, (EmptyTree,n))
 numRaiz (Tree p esq dir, n) = ((Tree (numPair p n) esq dir), ((Tree (numPair p n) esq dir), n+1))

numeraArvore :: State (Tree (a,Int), Int) ()
numeraArvore = 
 do
  tree <- numeraRaiz
  return ()

main :: IO ()
main = return ()