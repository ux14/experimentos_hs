import Data.List
import Control.Monad

type Node  = Int
type Edge  = Int
type Path = [Edge]

data Graph = Graph { nodes  :: [Node]
                    ,edges  :: [Edge]
                    ,source :: Edge -> Node
                    ,target :: Edge -> Node}

instance Show Graph where
    show (Graph n e s t) = let showSrcTgt [] = "";
                               showSrcTgt (ei:e) = (show ei) ++ " : " ++ (show $ s ei) ++ " -> " ++ (show $ t ei) ++ "\n" ++ (showSrcTgt e);
                           in show n ++ " " ++ show e ++ "\n" ++ (showSrcTgt e)

-- Constroi a função 'source' de um grafo a partir de sua
-- Lista de adjacência
sourceFromAdjList :: [(Edge,Node,Node)] -> (Edge -> Node)
sourceFromAdjList adjL = \e -> ((\(_,x,_) -> x) . (findTupleWithEdge e) $ adjL)
 where
 findTupleWithEdge e = head . filter (\(ei,_,_) -> ei == e)

-- Constroi a função 'target' de um grafo a partir de sua
-- Lista de adjacência
targetFromAdjList :: [(Edge,Node,Node)] -> (Edge -> Node)
targetFromAdjList adjL = \e -> ((\(_,_,x) -> x) . (findTupleWithEdge e) $ adjL)
 where
 findTupleWithEdge e = head . filter (\(ei,_,_) -> ei == e)

-- Lista de arestas que tem o nodo 'n' como fonte (source)
srcNode :: Graph -> Node -> [Edge]
srcNode g n = filter (\e -> source g e == n) $ edges g

-- Lista de arestas que tem o nodo 'n' como alvo (target)
tgtNode :: Graph -> Node -> [Edge]
tgtNode g n = filter (\e -> target g e == n) $ edges g

-- Dado uma lista de arestas, tenta extender com a aresta 'e'
-- Possível falha: alvo da última aresta é diferente da fonte
-- de 'e'
extendPath :: Graph -> Edge -> Path -> Path
extendPath g e [] = []
extendPath g e (ei:ek)
 | target g ei == source g e = e:ei:ek
 | otherwise = []

-- Trata uma lista de caminhos como uma computação não-determinística
-- e usa de propriedades de listas como Funtores Aplicativos e listas
-- como uma Monada, extendendo cada caminho por todas as arestas que
-- tem como fonte o alvo da última aresta no caminho (através de extendPath)
extendListOfPaths :: Graph -> [Path] -> [Path]
extendListOfPaths g paths = filter (\p -> p /= []) $ paths >>= (\p -> (extendPath g) <$> (edges g) <*> [p])

findNonRepPaths :: Graph -> Node -> Node -> [Path]
findNonRepPaths g ni nj = concat (caminhos g ni nj)

-- lista de lista de caminhos em que cada elemento é uma lista
-- de todos os caminhos com um número fixo de arestas de 1 até |e| 
-- arestas que começam em uma aresta com fonte ni e terminam em uma
-- aresta com alvo nj
caminhos :: Graph -> Node -> Node -> [[Path]]
caminhos g ni nj = map (caminho g ni nj) $ take (length $ edges g) $ [1..]

-- lista de caminhos com k arestas em que a primeira aresta 
-- tem como fonte ni e a última aresta tem como alvo nj 
-- e todas arestas tem como fonte o alvo da anterior
caminho :: Graph -> Node -> Node -> Int -> [Path]
caminho g ni nj k = filter (\(ei:ek) -> target g ei == nj) (pathsWithLength g (srcNode g ni) k)

-- Lista de caminhos que começam por alguma aresta
-- de 'ini' e tem k arestas.
pathsWithLength :: Graph -> [Edge] -> Int -> [Path]
pathsWithLength g ini k
 | k <= 1 = map (\e->[e]) ini
 | otherwise = extendListOfPaths g $ pathsWithLength g ini (k-1)

------------------------------------------------------------------------

-- Morfismo entre dois grafos graph1 -> graph2
data GraphMorphism = GraphMorphism { src :: Graph
                                    ,tgt :: Graph
                                    ,fv  :: Node -> Node
                                    ,fe  :: Edge -> Edge}

instance Show GraphMorphism where
    show (GraphMorphism g1 g2 n e) = (show g1) ++ (show g2) ++ ( show $ zipWith (,) (nodes g1) (map n $ nodes g1) ) ++ "\n" ++ ( show $ zipWith (,) (edges g1) (map e $ edges g1) ) ++ "\n"

instance Eq GraphMorphism where
    (==) g1 g2 = (show g1) == (show g2)

isHomomorphism :: GraphMorphism -> Bool
isHomomorphism gm = (commutes ( (fv gm) . source1) (source2 . (fe gm) ) e1) && (commutes ( (fv gm) . target1) (target2 . (fe gm) ) e1)
 where
 commutes fi fj e = (map fi e) == (map fj e)
 (g1,g2) = (src gm, tgt gm)
 (e1,source1,target1) = (edges g1, source g1, target g1)
 (e2,source2,target2) = (edges g2, source g2, target g2)

-- Função a partir de uma lista associativa.
assocListToFunc :: (Eq a) => [(a,b)] -> (a -> b)
assocListToFunc l a = let Just pair = find ((== a) . fst) l
                        in 
                        snd pair

-- Dada uma lista representando um conjunto,
-- retorna todas as listas feitas de elementos
-- desse conjunto com o tamanho da lista de 
-- elementos. (l^l elementos)
--
-- Exemplo: genAllListsFromSet [1,2] = [ [1,1], [1,2], [2,1], [2,2] ]
--
genAllListsFromSet :: (Eq a) => [a] -> [[a]]
genAllListsFromSet l = all n
 where
 n = length $ nub l
 listOfLists n = take n $ map (\x -> nub l) (repeat n)
 all n = foldl (\x y -> (\z w -> z:w) <$> y <*> x) ([[]]) (listOfLists n)

--
genAllListsFromSet2 :: (Eq a) => Int -> [a] -> [[a]]
genAllListsFromSet2 n l = sequence (replicate n l')
 where
 l' = nub l

-- Gera uma lista associativa a partir de duas
-- listas.
genAssocList :: [a] -> [b] -> [(a,b)]
genAssocList = zipWith (,)

genAllAssocList :: (Eq b) => [a] -> [b] -> [[(a,b)]]
genAllAssocList a b = map (genAssocList a) (genAllListsFromSet2 (length a) b)

-- Lista de todas as funções entre dois 'conjuntos' (representados por listas)
-- Cuidado que passar um valor que não está na primeira lista gera erro.
allFuncs :: (Eq a, Eq b) => [a] -> [b] -> [a -> b]
allFuncs a b = map assocListToFunc (genAllAssocList a b)

-- Lista com todos os morfismos entre dois grafos
allMorphisms :: Graph -> Graph -> [GraphMorphism]
allMorphisms g1 g2 = do
                         fv1 <- allFuncs n1 n2
                         fe1 <- allFuncs e1 e2
                         return (GraphMorphism g1 g2 fv1 fe1)
 where
 (n1,n2) = (nodes g1, nodes g2)
 (e1,e2) = (edges g1, edges g2)
    
allHomomorphisms :: Graph -> Graph -> [GraphMorphism]
allHomomorphisms g1 g2 = filter isHomomorphism (allMorphisms g1 g2) 

src1 :: Edge -> Node
src1 1 = 2
src1 2 = 3
src1 3 = 4
src1 _ = 0

tgt1 :: Edge -> Node
tgt1 1 = 1
tgt1 2 = 2
tgt1 3 = 3
tgt1 _ = 0

n1 :: [Node]
n1 = [1,2,3,4]

e1 :: [Edge]
e1 = [1,2,3]

g1 :: Graph
g1 = Graph n1 e1 src1 tgt1

src2 :: Edge -> Node
src2 1 = 1
src2 2 = 2
src2 3 = 3
src2 _ = 0

tgt2 :: Edge -> Node
tgt2 1 = 3
tgt2 2 = 4
tgt2 3 = 1
tgt2 _ = 0

n2 :: [Node]
n2 = [1,2,3,4]

e2 :: [Edge]
e2 = [1,2,3]

g2 :: Graph
g2 = Graph n2 e2 src2 tgt2

completeGraph :: Int -> Graph
completeGraph 0 = Graph [] [] id id
completeGraph n = Graph [0..n-1] (completeEdges n) (completeSrc n) (completeTgt n)
 where
 completeEdges n = [0..n*n-1]
 completeSrc n e = snd . head . filter ( (== e) . fst ) $ zipWith (,) (completeEdges n) $ map (\e -> div e n) (completeEdges n)
 completeTgt n e = snd . head . filter ( (== e) . fst ) $ zipWith (,) (completeEdges n) $ map (\e -> mod e n) (completeEdges n)

-- Restrições de arestas e vértices restE restN
-- Restrições de arestas: 
-- do tipo (e1,e2) := aresta e1 no grafo fonte do morfismo mapeia para a aresta e2 no grafo alvo do morfismo
-- Restrições de vértices:
-- do tipo (n1,n2) := vértice n1 no grafo fonte do morfismo mapeia para o vértice n2 no grafo alvo do morfismo

type RestE = (Edge,Edge)
type RestN = (Node,Node)

-- Testa uma restrição de aresta contra uma de vértice
src_tgt_map :: Graph -> Graph -> RestE -> RestN -> Bool
src_tgt_map g1 g2 (re1,re2) (rn1,rn2)
 | s1 && t1 = s2 && t2 
 | s1 = s2
 | t1 = t2 
 | otherwise = True
 where
 s1 = source g1 re1 == rn1
 s2 = source g2 re2 == rn2
 t1 = target g1 re1 == rn1
 t2 = target g2 re2 == rn2

-- Testa uma restrição de aresta contra todas as de vértices
testRestrictE :: Graph -> Graph -> RestE -> [RestN] -> Bool
testRestrictE g1 g2 rE rN = and $ map (src_tgt_map g1 g2 rE) rN

testRestrictN :: Graph -> Graph -> [RestN] -> RestN -> Bool
testRestrictN g1 g2 rN (n1,n2) = (== []) $ filter (\(p1,p2) -> p1 == n1 && p2 /= n2) rN

-- Dada listas de restrições sobre dois grafos,
-- Retorna se ela é válida.
-- É válida, se toda aresta e1 mapeada em e2 tem seus
-- alvo e fonte mapeados nos alvo e fonte de e2, respectivamente
isRestrictionValid :: Graph -> Graph -> ([RestN],[RestE]) -> Bool
isRestrictionValid g1 g2 (rN,rE) = (and $ map (testRestrictN g1 g2 rN) rN) && (and $ map (testRestrictInv g1 g2 rN) rE)
 where
 testRestrictInv g g' rn re = testRestrictE g g' re rn

allValidRestList :: Graph -> Graph -> ([RestN],[RestE]) -> [([RestN],[RestE])]

allValidRestList (Graph _ [] _ _) g2 rNE = return rNE

allValidRestList g1 g2 (rN,rE) =  let
                                      n = nodes g1
                                      ei:e = edges g1
                                      s1 = source g1
                                      s2 = source g2
                                      t1 = target g1
                                      t2 = target g2
                                  in
                                  
                                  do    
                                      e2 <- (edges g2)
                                      
                                      let
                                          newRE = nub $ (ei,e2):rE
                                          newRN = nub $ (s1 ei,s2 e2):(t1 ei, t2 e2):rN
                                          in 

                                          if isRestrictionValid g1 g2 (newRN, newRE)
                                          then allValidRestList (Graph n e s1 t1) g2 (newRN, newRE)
                                          else []

diff :: (Eq a) => [a] -> [a] -> [a]
diff x [] = x
diff x l = diff (delete (head l) x) (tail l)

completeRestList :: Graph -> Graph -> ([RestN],[RestE]) -> [([RestN],[RestE])]
completeRestList g1 g2 (rN,rE) = let 
                                     remainingN = diff (nodes g1) (map fst rN)
                                 in
                                 if remainingN == []
                                     then return (rN,rE)
                                     else
                                         do
                                         n1 <- diff (nodes g1) (map fst rN)
                                         n2 <- (nodes g2)
                                         completeRestList g1 g2 ((n1,n2):rN, rE)

allHomomorphisms2 :: Graph -> Graph -> [GraphMorphism]
allHomomorphisms2 g1 g2 = map (toMorphism g1 g2) restrics
 where
  toMorphism g1 g2 (rN,rE) = GraphMorphism g1 g2 (assocListToFunc rN) (assocListToFunc rE)
  restrics = concatMap (completeRestList g1 g2) $ allValidRestList g1 g2 ([],[])

-- src m1 == tgt m2
compositionMorphism :: GraphMorphism -> GraphMorphism -> GraphMorphism
compositionMorphism m1 m2 = GraphMorphism (src m2) (tgt m1) (fv m1 . fv m2) (fe m1 . fe m2)

splitOn :: (Eq a) => a -> [a] -> ([a],[a])
splitOn _ [] = ([],[])
splitOn c (f:rest) = if c == f then ([],rest)
                               else ( f:(fst $ splitOn c rest), (snd $ splitOn c rest))

getGraph :: IO Graph
getGraph = do
    nodeLine <- getLine
    edgeLine <- getLine
    let nodeNum = (read nodeLine) :: Int
    let edgeNum = (read edgeLine) :: Int

    srcEdge <- forM [1..edgeNum] (\_ -> do
        edge_src <- getLine
        let (e,s) = splitOn ',' $ map (\c -> if (c == '(' || c == ')') then ' ' else c) edge_src
        return ( read e :: Int, read s :: Int))

    tgtEdge <- forM [1..edgeNum] (\_ -> do
        edge_tgt <- getLine
        let (e,t) = splitOn ',' $ map (\c -> if (c == '(' || c == ')') then ' ' else c) edge_tgt
        return ( read e :: Int, read t :: Int))

    return $ Graph [1..nodeNum] [1..edgeNum] (assocListToFunc srcEdge) (assocListToFunc tgtEdge)


main :: IO ()
main = do
--	numGraphLine <- getLine
--	let numGraph = (read numGraphLine) :: Int
    g1 <- getGraph
    g2 <- getGraph
    let k2 = completeGraph 2
    putStr $ show (allHomomorphisms g1 g2)