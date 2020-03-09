module Exame1819 where

    isSorted :: (Ord a) => [a] -> Bool
    isSorted [] = True
    isSorted (a:as:[])
        |a<as = True
        |otherwise = False
    isSorted (a:as:next)
        |a<as = isSorted (as:next)
        |otherwise = False
    
    inits' :: [a] -> [[a]]
    inits' [] = [[]]
    inits' a = inits' (init a) ++ [a]

    maximumMB :: (Ord a) => [Maybe a] -> Maybe a
    maximumMB [] = Nothing
    maximumMB (a:as) = maximumMB_aux Nothing a as
    -- auxiliar
    maximumMB_aux x Nothing (a:as) = maximumMB_aux x a as
    maximumMB_aux x a []
        |x>a = x
        |otherwise = a
    maximumMB_aux x a (b:bs)
        |a>x = maximumMB_aux a b bs
        |otherwise = maximumMB_aux x b bs

    data LTree a = Tip a | Fork (LTree a) (LTree a)

    listaLT :: LTree a -> [a]
    listaLT (Tip a) = [a]
    listaLT (Fork a b) = listaLT a ++ listaLT b
    instance (Show a) => Show (LTree a) where
        show (Tip a) = show a ++ "\n"
        show (Fork a b) = mostra 1 a ++ mostra 1 b
    mostra :: (Show a) => Int -> LTree a -> String
    mostra n (Tip a) = replicate n '.' ++ show a ++ "\n"
    mostra n (Fork a b) = mostra (n + 1) a ++ mostra (n + 1) b

    maxSumInit :: (Num a, Ord a) => [a] -> a
    maxSumInit l = maximum [sum m | m <- inits' l]
