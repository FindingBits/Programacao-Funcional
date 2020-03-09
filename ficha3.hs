module Ficha3 where
import Ficha1
import Ficha2
type Poligonal = [Ponto]

comp :: Poligonal -> Double
comp (p1:p2:next)=
    (dist p1 p2)+comp(p2:next)
comp _ = 0

dist :: Ponto -> Ponto -> Double

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 =
    ordena (normaliza p1)==ordena (normaliza p2)

efechada :: Poligonal -> Bool
efechada [] = Falsetake ::  Int -> [a] -> [a]
efechada l1 = head l1 = last l1

triangulo :: Poligonal -> [Figura]
triangulo (p1:p2:p3:p4:next)
    (triangulo p1 p2 p3):(triangulo(p1:p4:p4:next))
triangulo _ = []

areaP :: Poligonal -> Double
areaP lp = let lp = triangulo lp
            in somaArea lp
                |somaArea :: [Figura] -> Double
                |somaArea [] = 0
                |somaArea (h:next)=(ordena h)+somaArea next
