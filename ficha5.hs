module Ficha5 where
    -- FICHA 5

    -- (EX1)
    anyMy :: (a -> Bool) -> [a] -> Bool
    anyMy f [] = False
    anyMy f (a:as)
        |f (a) = True  
        |otherwise = anyMy f (as)

    zipWithMy :: (a->b->c) -> [a] -> [b] -> [c]
    zipWithMy f [] [] = []
    zipWithMy f _ [] = []
    zipWithMy f [] _ = []
    zipWithMy f (a:as) (b:bs) = (f a b):zipWithMy f as bs

    takeWhileMy :: (a->Bool) -> [a] -> [a]
    takeWhileMy f [] = []
    takeWhileMy f (a:as)
        |(f (a)) == True = a:takeWhileMy f as
        |otherwise = takeWhileMy f as

    dropWhileMy :: (a->Bool) -> [a] -> [a]
    dropWhileMy f [] = []
    dropWhileMy f (a:as)
        |(f (a)) == True = dropWhileMy f as
        |otherwise = a:dropWhileMy f as
    
    spanMy :: (a-> Bool) -> [a] -> ([a],[a])
    spanMy f a = (takeWhileMy f a,dropWhileMy f a)

    deleteByMy :: (a -> a -> Bool) -> a -> [a] -> [a]
    deleteByMy f elem [] = []
    deleteByMy f elem (a:as)
        |(f elem a) == True = deleteByMy f elem as
        |otherwise = a:deleteByMy f elem as

    sortOnMy :: Ord b => (a -> b) -> [a] -> [a]
    sortOnMy f [] = []
    sortOnMy f (a:as) = insertMy f a (sortOnMy f as)
    -- auxiliar
    insertMy f elem [] = [elem]
    insertMy f elem (a:as)
        |(f elem)>(f a) = a:insertMy f elem as
        |otherwise = (elem:a:as)
    
    -- (EX2)
    type Polinomio = [Monomio]
    type Monomio = (Float,Int)

    selgrau :: Int -> Polinomio -> Polinomio
    selgrau g [] = []
    selgrau g ((b,e):as)    
        |g==e = (b,e):selgrau g as
        |otherwise = selgrau g as
    
    conta :: Int -> Polinomio -> Int
    conta g [] = 0
    conta g  ((b,e):as)
        |g==e = 1+conta g as
        |otherwise = conta g as

    grau :: Polinomio -> Int
    grau [] = 0
    grau ((b,e):as) = grauAux 0 ((b,e):as)
    -- auxiliar
    grauAux g [] = g
    grauAux g ((b,e):as)
        |e>=g = grauAux e as
        |otherwise = grauAux g as

    deriv :: Polinomio -> Polinomio
    deriv [] = []
    deriv ((b,e):as)
        |e>0 = (b+1,e-1):deriv as
        |otherwise = (0,0):deriv as
    
    calcula :: Float -> Polinomio -> Float
    calcula 0 _ = 0
    calcula v [] = v
    calcula v ((b,e):as) = ((v^e)*b)+calcula v as

    simp :: Polinomio -> Polinomio
    simp [] = []
    simp ((b,e):as)
        |e==0 = simp as
        |otherwise = (b,e):simp as

    mult :: Monomio -> Polinomio -> Polinomio
    mult _ [] = []
    mult (bm,em) ((b,e):as) = (bm*b,em+e):mult (bm,em) as

    ordena :: Polinomio -> Polinomio
    ordena a = sortOnMy snd a

    normaliza :: Polinomio -> Polinomio
    normaliza l = let x = fromIntegral $ grau l in [ ((a/x),b) | (a,b) <- l]

    soma :: Polinomio -> Polinomio -> Polinomio
    soma p1 p2 = normaliza (p1 ++ p2)

    produto :: Polinomio -> Polinomio -> Polinomio
    produto p1 p2 = normaliza $ concat [ mult (a, b) p2 | (a, b) <- p1]

    -- Não percebo
    --equiv :: Polinomio -> Polinomio -> Bool

    -- (EX3)
    type Mat a = [[a]]

    dimOK :: Mat a -> Bool
    dimOK [[]] = True
    dimOK (a:as) = dimOKAux (length a) as
    -- auxiliar
    dimOKAux l [] = True
    dimOKAux l (a:as)
        |length a == l = dimOKAux l as
        |otherwise = False
    
    -- assumo que quer n de linhas e colunas
    dimMat :: Mat a -> (Int,Int)
    dimMat [[]] = (0,0)
    dimMat (a:as) = (length a, length (a:as))
