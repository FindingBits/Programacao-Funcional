module Ficha2 where
-- exe 4
-- next <-> tail <-> t
type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta grau [] = 0
conta grau ((x1,l1):next)
    |grau==l1=1+conta grau next
    |grau/=l1=conta grau next

grau :: Polinomio -> Int
grau [(x,l)]=l
grau ((x1,l1):((x2,l2):next))=
    if(l1<l2) then grau ((x2,l2):next)
    else grau ((x1,l1):next)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau grau ((x1,l1):next)
    |grau==l1=((x1,l1):selgrau grau next)
    |grau/=l1=selgrau grau next

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x1,l1):next)
    |l1==0=deriv next
    |l1/=0=(x1*(fromIntegral l1),l1-1):deriv next

calcula :: Float -> Polinomio -> Float
calcula valor [] = 0
calcula valor ((x1,l1):next)=x1*(valor^l1)+calcula valor next

mult :: Monomio -> Polinomio -> Polinomio
mult (x1,l1) [] = []
mult (x1,l1) ((x2,l2):next)=(x1*x2,l1+l2):(mult (x1,l1) next)

remgrau :: Int -> Polinomio -> Polinomio
remgrau grau [] = []
remgrau grau ((x1,l1):next)
    |grau==l1=remgrau grau next
    |grau/=l1=(x1,l1):remgrau grau next

somaCoef :: Polinomio -> Float
somacoef [] = []
somaCoef ((x1,l1):next)=x1+somaCoef next

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((x1,l1):next) =
    let lgi = remgrau l1 next
        lgd = remgrau l1 next
        x2 = somaCoef ((x1,l1):lgi)
    in if(x2==0) then normaliza lgd
        else (x2,l1):(normaliza lgd)

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza(p1++p2)

produto ::  Polinomio -> Polinomio -> Polinomio
produto _ [] = []
produto [] _ = []
produto p1 p2 = normaliza(prod p1 p2)
    where prod [] p2 = []
          prod ((x1,l1):next) p2 =
            (mult (x1,l1) p2)++(prod p1 p2)

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((x1,l1):next)=insere (x1,l1) (ordena next)
    where insere :: Monomio -> Polinomio -> Polinomio
          insere (x1,l1) [] = [(x1,l1)]
          insere (x1,l1) ((x2,l2):next)
                |l1<=l2=(x1,l1):(x2,l2):next
                |otherwise=(x2,l2):insere (x1,l1) next

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena(normaliza p1)==ordena(normaliza p2)

