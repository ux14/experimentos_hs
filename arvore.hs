data Tree = EmptyTree | Tree {root :: Int, left :: Tree, right :: Tree}

singleton :: Int -> Tree
singleton x = Tree x EmptyTree EmptyTree

findInTree :: Int -> Tree -> Bool
findInTree _ EmptyTree = False
findInTree x tree = (x == (root tree)) || (findInTree x (left tree)) || (findInTree x (right tree))

uniqueNumAux :: (Tree,Int) -> (Tree,Int)
uniqueNumAux (EmptyTree,curr) = (EmptyTree,curr)
uniqueNumAux (tree,curr) = (Tree next renumTreeLeft renumRightTree, last)
 where 
 next = curr + 1
 aux1 = uniqueNumAux (left tree, next)
 aux2 = uniqueNumAux (right tree, snd $ aux1)
 last = snd aux2
 renumTreeLeft = fst $ aux1
 renumRightTree = fst $ aux2

uniqueNum :: Tree -> Tree
uniqueNum t = fst $ uniqueNumAux (t,0)

listFromTree :: Tree -> [Int]
listFromTree EmptyTree = []
listFromTree tree = (root tree) : (listFromTree $ left tree) ++ (listFromTree $ right tree)

import Control.Monad.State