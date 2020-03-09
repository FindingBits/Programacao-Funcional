module Cinquenta where
    -- 1) Apresente uma funcao enumFromTo
    enumFromToMy :: Int -> Int -> [Int]
    enumFromToMy x y
        |y==0 = []
        |y<=x = [x]
        |otherwise = x:enumFromToMy (x+1) y
    
    -- 2) Apresente uma funcao enumFromThenTo
    enumFromThenToMy :: Int -> Int -> Int -> [Int]
    enumFromThenToMy x y z
        |y==0 = []
        |z<y = [x]
        |otherwise = x:enumFromThenToMy y (2 * y - x) z

    -- 3) Apresente uma funcao (++)
    plusPlus :: [a] -> [a] -> [a]
    plusPlus a [] = a
    plusPlus [] b = b
    plusPlus (a:as) (b:bs) = a:b:plusPlus as bs

    -- 4) Apresente uma funcao (!!)
    exclamation :: [a] -> Int -> a
    exclamation a x
        |x==0 = (head a)
        |otherwise = exclamation (tail a) (x-1)

    -- 5) Apresente uma funcao reverse
    reverseMy :: [a] -> [a]
    reverseMy [] = []
    reverseMy (a:as) = aux a (reverseMy as)
                where aux :: a -> [a] -> [a]
                      aux x [] = [x]
                      aux x (a:as) = a:aux x as

    -- 6) Apresente uma funcao take
    takeMy :: Int -> [a] -> [a]
    takeMy 0 _ = []
    takeMy _ [] = []
    takeMy x a
        |length(a)==x = a
        |otherwise = takeMy x (init a)
    
    -- 7) Apresente uma funcao drop
    dropMy :: Int -> [a] -> [a]
    dropMy 0 a = a
    dropMy _ [] = []
    dropMy x a
        |x==0 = a
        |otherwise = dropMy (x-1) (tail a)
    
    -- 8) Apresente uma funcao zip
    zipMy :: [a] -> [b] -> [(a,b)]
    zipMy [] [] = []
    zipMy (a:as) (b:bs) = (a,b):zipMy as bs

    -- 9) Apresente uma funcao elem
    elemMy :: Eq a => a -> [a] -> Bool
    elemMy _ [] = False
    elemMy x (a:as)
        |x==a = True
        |otherwise = elemMy x as
    
    -- 10) Apresente uma funcao replicate
    replicateMy :: Int -> a -> [a]
    replicateMy 0 _ = []
    replicateMy x a
        |x>0 = a:replicateMy (x-1) a
        |otherwise=[]

    -- 11) Apresente uma função intersperce
    intersperceMy ::  a -> [a] -> [a]
    intersperceMy _ [] = []
    intersperceMy _ [a] = [a]
    intersperceMy x (a:as) = a:(x:intersperceMy x as)

    -- 12) Apresente uma função group
    groupMy ::  Eq a => [a] -> [[a]]
    groupMy [] = []
    groupMy a = auxG:groupMy (dropMy (length auxG) a)
            where auxG = groupMy_aux a
    groupMy_aux :: Eq a => [a] -> [a]
    groupMy_aux [a] = [a]
    groupMy_aux (a:as:ass)
        |a==as = a:groupMy_aux (as:ass)
        |otherwise = [a]

    -- 13) Apresente uma função concat
    concatMy :: [[a]] -> [a]
    concatMy [] = []
    concatMy (a:as) = plusPlus a (concatMy as)

    -- 14) Apresente uma função inits
    initsMy :: [a] -> [[a]]
    initsMy a = initsMy_aux 0 a
    initsMy_aux :: Int -> [a] -> [[a]]
    initsMy_aux _ [] = [[]]
    initsMy_aux x a
        |x<(length a) = takeMy x a:initsMy_aux (x+1) a
        |otherwise = []

    -- 15) Apresente uma função tails
    tailsMy :: [a] -> [[a]]
    tailsMy a = tailsMy_aux 0 a
    tailsMy_aux :: Int -> [a] -> [[a]]
    tailsMy_aux _ [] = [[]]
    tailsMy_aux x a
        |x<(length a) = dropMy x a : tailsMy_aux (x+1) a
        |otherwise = []

    -- 16) Apresente uma função isPrefixOf
    isPrefixOfMy :: Eq a => [a] -> [a] -> Bool
    isPrefixOfMy [] _ = True
    isPrefixOfMy _ [] = False
    isPrefixOfMy (a1:as1) (a2:as2)
        |a1==a2 = isPrefixOfMy as1 as2
        |otherwise = False
    
    --17) Apresente uma função isSuffixOf
    isSuffixOfMy ::  Eq a => [a]-> [a] -> Bool
    isSuffixOfMy [] _ = False
    isSuffixOfMy _ [] = True
    isSuffixOfMy a b = isPrefixOfMy (reverseMy a) (reverseMy b)

    -- 18) Apresente uma função isSubsequenceOf
    isSubsequenceOfMy ::  Eq a =>[a] -> [a] -> Bool
    isSubsequenceOfMy _ [] = False
    isSubsequenceOfMy [] _ = True
    isSubsequenceOfMy (a1:as1) (a2:as2)
        |a1==a2 = isSubsequenceOfMy as1 as2
        |otherwise = isSubsequenceOfMy (a1:as1) as2

    -- 19) Apresente uma função elemIndices
    elemIndicesMy ::  Eq a => a ->[a] -> [Int]
    elemIndicesMy _ [] = []
    elemIndicesMy x a = elemIndicesMy_aux 0 x a
    elemIndicesMy_aux :: Eq a => Int -> a -> [a] -> [Int]
    elemIndicesMy_aux _ _ [] = []
    elemIndicesMy_aux i x (a:as)
        |x==a = i:elemIndicesMy_aux (i+1) x as
        |otherwise = elemIndicesMy_aux (i+1) x as

    -- 20) Apresente uma função nub
    nubMy ::  Eq a => [a] -> [a]
    nubMy [] = []
    nubMy (a:as) = a:nubMy (nubMy_aux a as)
    nubMy_aux :: Eq a => a -> [a] -> [a]
    nubMy_aux _ [] = []
    nubMy_aux x (a:as)
        |x==a = nubMy_aux x as
        |otherwise = a:nubMy_aux x as
    
    -- 21) Apresente uma função delete
    deleteMy ::  Eq a => a -> [a]-> [a]
    deleteMy _ [] = []
    deleteMy x (a:as)
        |x==a = as
        |otherwise = a:deleteMy x as
    
    -- 22) Apresente uma função (\\)
    doubleSlashMy :: Eq a => [a] -> [a]-> [a]
    doubleSlashMy a [] = a
    doubleSlashMy [] _ = []
    doubleSlashMy a (b:bs) = (doubleSlashMy (deleteMy b a) bs)

    -- 23) Apresente uma função union
    unionMy :: Eq a => [a] -> [a] -> [a]
    unionMy a [] = a
    unionMy [] a = a
    unionMy a (b:bs) =
        if(elemMy b a) then unionMy a bs
        else unionMy (plusPlus a [b]) bs
    
    -- 24) Apresente uma função intersect
    intersectMy ::  Eq a => [a] ->[a] -> [a]
    intersectMy _ [] = []
    intersectMy [] _ = []
    intersectMy a (b:bs) =
        if(elemMy b a) then b:intersectMy a bs
        else intersectMy a bs
    
    -- 25) Apresente uma função insert
    insertMy :: Ord a => a -> [a] -> [a]
    insertMy x [] = [x]
    insertMy x (a:as)
        |x>a = (a:insertMy x as)
        |otherwise = plusPlus [x] (a:as)
    
    -- 26) Apresente uma função unwords 
    unwordsMy :: [String] -> String
    unwordsMy [] = ""
    unwordsMy [x] = x
    unwordsMy (a:as) = plusPlus (plusPlus a " ") (unwordsMy as)