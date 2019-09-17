import Control.Monad.State

data Tree a = EmptyTree | Tree {root :: a, left :: Tree a, right :: Tree a} deriving (Show,Read)

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

numeraArvore :: Tree a -> State Int (Tree (a,Int))
numeraArvore EmptyTree = return EmptyTree
numeraArvore (Tree x l r) = do
                            atual <- get
                            put (atual+1)

                            lTree <- numeraArvore l
                            rTree <- numeraArvore r

                            return (Tree (x,atual) lTree rTree)

t1 :: Tree Int
t1 = Tree 1 EmptyTree EmptyTree

t2 :: Tree Int
t2 = Tree 2 EmptyTree EmptyTree

t3 :: Tree Int
t3 = Tree 3 t1 t2

t4 :: Tree Int
t4 = Tree 4 t2 EmptyTree

t5 :: Tree Int
t5 = Tree 5 t4 t3

main :: IO ()
main = return ()