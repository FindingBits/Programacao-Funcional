module Exame1718 where
    import System.IO
    exex :: [a] -> Int -> a
    exex l p = c_aux 0 l p
    -- auxiliar
    c_aux x (h:t) p
        |p==x = h
        |otherwise = c_aux (x+1) t p

    data Movimento = Norte | Sul | Este | Oeste deriving Show

    posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
    posicao x [] = x
    posicao (x,y) (Norte:t) = posicao (x,y+1) t
    posicao (x,y) (Sul:t) = posicao (x,y-1) t
    posicao (x,y) (Este:t) = posicao (x+1,y) t
    posicao (x,y) (Oeste:t) = posicao (x-1,y) t

    any' :: (a -> Bool) -> [a] -> Bool
    any' f [] = False
    any' f (h:t)
        |f (h) == True = True
        |otherwise = any' f t 
    
    type Mat a = [[a]]

    --triSup :: Num a => Mat a -> Bool
    triSup [[]] = True
    triSup (h:t) = triSup_aux 0 0 h t
    -- auxiliar
    triSup_aux _ _ [] [] = True
    triSup_aux cs ca (sh:st) (h:t)
        |ca>cs = False
        |sh==0 = triSup_aux cs (ca+1) (st) (h:t)
        |sh/=0 && cs/=ca = False
        |otherwise = triSup_aux (cs+1) 0 h (t)

    
    movimenta :: IO (Int,Int)
    movimenta = do
                line <- getLine
                return (findOut (0,0) line)

    findOut :: (Int,Int) -> String -> (Int,Int)
    findOut x [] = x
    findOut (x,y) (h:t)
        |h=='N' = findOut (x,y+1) t
        |h=='S' = findOut (x,y-1) t
        |h=='E' = findOut (x+1,y) t
        |h=='O' = findOut (x-1,y) t
        |otherwise = (x,y)
    
    data Imagem = Quadrado Int | Mover (Int,Int) Imagem | Juntar [Imagem]

    instance Eq (Imagem) where
        (Quadrado b1) == (Quadrado b2) = True
        (Mover x1 e1) == (Mover x2 e2) = (x1==x2) && (e1==e2)
        (Juntar c1) == (Juntar c2) = True
        _ == _ = False