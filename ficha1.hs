-- ficha1
module Ficha1 where

    perimetro :: Float -> Float
    perimetro x = x*pi
    
    dist :: (Double,Double) -> (Double,Double) -> Double
    dist (x1,l1) (x2,l2) = sqrt(((x1-x2)^2)+((l1-l2)^2))

    primUlt :: [Int] -> [Int]
    primUlt [] = []
    primUlt xs = [head xs,last xs]

    multiplo :: Int -> Int -> Bool
    multiplo m n
        |(mod m n)==0 = True
        |(mod m n)/=0 = False

    truncaImpar :: [Int] -> [Int]
    truncaImpar xs
        |(mod (length xs) 2)==0 = xs
        |(mod (length xs) 2)/=0 = tail xs
    
    max2 :: Int -> Int -> Int
    max2 x y
        |x>y = x
        |otherwise = y
    
    max3 :: Int -> Int -> Int -> Int
    max3 x y z
        |(max2 x y)>z = (max2 x y)
        |otherwise = z
    
    fResolvente :: Int -> Int -> Int -> [Int]
    xs=[]
    s1 = Nothing
    s2 = Nothing
    fResolvente x2 x1 y
        |x2==0 = []
        |otherwise = do {
            
            s1 = (-x1+sqrt((x1^2)-(4*x2*y)))/(2*x2)
            s2 = (-x1+sqrt((x1^2)-(4*x2*y)))/(2*x2)
            if(s1/=Nothing) then xs:[s1]
            else if(s2/=Nothing) then xs:[s2]
        }

    -- nRaizes :: Int -> Int -> Int -> Int