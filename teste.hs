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
	show (GraphMorphism g1 g2 n e) = (show g1) ++ (show g2) ++ ( show $ zipWith (,) (nodes g1) (map n $ nodes g1) ) ++ "\n" ++ ( show $ zipWith (,) (edges g1) (map e $ edges g1) )

isHomomorphism :: GraphMorphism -> Bool
isHomomorphism gm = (commutes ( (fv gm) . source1) (source2 . (fe gm) ) e1) && (commutes ( (fv gm) . target1) (target2 . (fe gm) ) e1)
 where
 commutes fi fj e = (map fi e) == (map fj e)
 (g1,g2) = (src gm, tgt gm)
 (e1,source1,target1) = (edges g1, source g1, target g1)
 (e2,source2,target2) = (edges g2, source g2, target g2)

-- find
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (xi:x) = if (f xi) then (Just xi) else (find f x)

-- Função a partir de uma lista associativa.
assocListToFunc :: (Eq a) => [(a,b)] -> (a -> b)
assocListToFunc l a = let Just pair = find (\p -> fst p == a) l
						in 
						snd pair

-- Dada uma lista representando um conjunto,
-- retorna todas as listas feitas de elementos
-- desse conjunto com o tamanho da lista de 
-- elementos. (l^l elementos)
--
-- Exemplo: genAllListsFromSet [1,2] = [ [1,1], [1,2], [2,1], [2,2] ]
--
genAllListsFromSet :: [a] -> [[a]]
genAllListsFromSet l = all n
 where
 n = length l
 listOfLists n = take n $ map (\x -> l) (repeat n)
 all n = foldl (\x y -> (\z w -> z:w) <$> y <*> x) ([[]]) (listOfLists n)

-- Gera uma lista associativa a partir de duas
-- listas.
genAssocList :: [a] -> [b] -> [(a,b)]
genAssocList = zipWith (\f s -> (f,s))

genAllAssocList :: [a] -> [b] -> [[(a,b)]]
genAllAssocList a b = map (genAssocList a) (genAllListsFromSet b)

allFuncs :: (Eq a) => [a] -> [b] -> [a -> b]
allFuncs a b = map assocListToFunc (genAllAssocList a b)

allMorphisms :: Graph -> Graph -> [GraphMorphism]
allMorphisms g1 g2 = (GraphMorphism g1 g2) <$> (allFuncs n1 n2) <*> (allFuncs e1 e2)
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