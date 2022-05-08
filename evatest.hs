--ejercicio 1

borrarUlt:: [a] -> [a]
borrarUlt [] = []
borrarUlt (x:[]) = []
borrarUlt (x:xs) = x:borrarUlt xs

contDups:: (Eq a) => [a] -> [a]
contDups [] = []
contDups (x:[]) = [x]
contDups (x:y:xs) = if x == y then contDups (x:xs) else x:contDups (y:xs)

serie2:: [a] -> [[a]]
serie2 [] = []
serie2 (xs) = xs:serie2 (reverse (tail (reverse xs)))

serie:: [a] -> [[a]]
serie xs = []:(reverse (serie2 xs))

--ejercicio 2

is5 x = x==5

ej2a:: (Int -> Bool) -> (Int -> Bool)
ej2a x y = x y

ej2b' x = x == 3

ej2b:: Bool -> (Int->Bool)
ej2b x = if x then ej2b' else ej2b'

--ejercicio 3

divisors x = [i | i <- [1..(x-1)], (mod x i) == 0]

sumadiv [] = 0
sumadiv (x:xs) = x + sumadiv xs

perfectNums = [x | x <- [1..10],x == sumadiv (divisors x)]

expandir xs = [x | x <- xs, i <- [1..x]]

--ejercicio 4

data AD a = Respuesta a |Opciones [AD a] deriving(Show)

ad1 = Opciones [(Respuesta 2),(Respuesta 2),(Respuesta 2),(Respuesta 2),(Opciones [(Respuesta 4)])]

{-
prof f (Respuesta x) = f x
prof f (Opciones (x:xs)) = prof f x:prof' f xs

prof' f [] = []
prof' f (x:xs) = prof f x:prof' f xs
-}