module Ficha6 where
    data BTree a = Empty | Node a (BTree a) (BTree a)
                    deriving Show
    -- (EX1)
    -- minha mini arvore
    arvore = (Node 1 Empty (Node 2 Empty (Node 3 Empty Empty))) 

    altura :: BTree a -> Int
    altura Empty = 0
    altura (Node _ a b) = max (1 + altura a) (1 + altura b)

    contaNodos :: BTree a -> Int
    contaNodos Empty = 0
    contaNodos (Node _ a b) = 1+(contaNodos a)+(contaNodos b)

    -- numero de folhas da arvore (Empty before node)
    folhas :: BTree a -> Int
    folhas Empty = 0
    folhas (Node _ Empty Empty) = 1
    folhas (Node _ a b) = folhas a + folhas b

    -- sao removidos elementos apartir de uma profundidade
    prune :: Int -> BTree a -> BTree a
    prune _ Empty = Empty
    prune 0 _ = Empty
    prune x (Node e l r) = Node e (prune (x - 1) l) (prune (x - 1) r)

    -- dá o caminho que foi percorrido na arvore
    path :: [Bool] -> BTree a -> [a]
    path _ Empty = []
    path [] (Node e l r) = [e]
    path (a:as) (Node e l r) = e:path as (if a then r else l)

    -- espelho da arvore, o que esta do lado esquerdo
    -- ficad do lado direito e vice versa
    mirror :: BTree a -> BTree a
    mirror Empty = Empty
    mirror (Node e l r) = Node e (mirror r) (mirror l)

    -- funcao zipWith para arvores
    -- ajuda nestas duas
    zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
    zipWithBT f (Node e l r) (Node a b c) = Node (f e a) (zipWithBT f l b) (zipWithBT f r c)
    zipWithBT _ _ _ = Empty

    unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
    unzipBT Empty = (Empty, Empty, Empty)
    unzipBT (Node (a,b,c) l r) = (Node a unzipL1 unzipR1,Node b unzipL2 unzipR2,Node c unzipL3 unzipR3)
        where (unzipL1,unzipL2,unzipL3) = unzipBT l
              (unzipR1,unzipR2,unzipR3) = unzipBT r
    
    -- (Ex2)
    minimo :: Ord a => BTree a -> a
    minimo (Node e Empty _) = e
    minimo (Node e l r) = minimo l

    semMinimo :: Ord a => BTree a -> BTree a
    semMinimo (Node _ Empty _) = Empty
    semMinimo (Node e l r) = Node e (semMinimo l) r

    minSmin :: Ord a => BTree a -> (a,BTree a)
    minSmin (Node e Empty _) = (e,Empty)
    minSmin (Node e l r) = (a,Node e b r)
        where (a,b) = minSmin l

    --remove :: Ord a => a -> BTree a -> BTree a
    --remove _ Empty = Empty
    --remove x (Node e l r) | x < e = Node e (remove x l) r
    --                    | x > e = Node e l (remove x r)
    --                    | otherwise = aux x (Node e l r)
    --    where aux n (Node a b c) = case b of Empty -> c
    --                                         otherwise -> case c of Empty -> b
    --                                                                otherwise -> Node g b h
    --          (g,h) = minSmin r                                                                otherwise -> Node g b h (g,h) = minSmin r
    
    --(Ex3)
    type Aluno = (Numero,Nome,Regime,Classificacao)
    type Numero = Int
    type Nome = String
    data Regime = ORD | TE | MEL  deriving Show
    data Classificacao = Aprov Int | Rep | Faltou deriving Show
    type Turma = BTree Aluno  --  árvore binária de procura (ordenada por número)

    turma1 :: Turma
    turma1 = (Node (15,"Luís",ORD,Aprov 14) (Node (12,"Joana",MEL,Faltou) (Node (7,"Diogo",TE,Rep) Empty
                                                                                                Empty) 
                                                                        (Node (14,"Lara",ORD,Aprov 19) Empty
                                                                                                        Empty))
                                            (Node (20,"Pedro",TE,Aprov 10) Empty
                                                                        (Node (25,"Sofia",ORD,Aprov 20) (Node (23,"Rita",ORD,Aprov 17) Empty
                                                                                                                                        Empty)
                                                                                                        (Node (28,"Vasco",MEL,Rep) Empty
                                                                                                                                    Empty))))

