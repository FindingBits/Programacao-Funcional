doubleMe x = x+x
doubleUs x y = x*2+y*2
doubleSmallNumber x = if(x>100)
    then x
    else x*2
doubleSmallNumber' x = (if(x>100) then x else x*2)+1
conanO'Brien = "It's a-me, Conan O'Brien!"
boomBangs xs = [if(x<10) then "BOOM!" else "BANG!" | x<-xs, odd x]
stayOut y = [x | x<-[10..20], x/=y]
nouns = ["hobo","frog","pope"]
adjectives = ["lazy","grouchy","scheming"]
joinBothNames = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
lengthMyVer xs = sum[1 | _ <- xs]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

circumference :: Float -> Float
circumference r = 2*pi*r

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n*factorial(n-1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

bmiTell :: (RealFloat a) => a -> a-> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
    where bmi = weight/height^2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted
