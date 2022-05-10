--ejercicio 1

borrarUlt:: [a] -> [a]
borrarUlt [] = []
borrarUlt (x:[]) = []
borrarUlt (x:xs) = x:borrarUlt xs

contDups2:: (Eq a) => [a] -> [a]
contDups2 xs = [x | (x,y) <- zip xs (tail xs), x /= y]

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

ad1:: AD Int
ad1 = Opciones [(Opciones [(Respuesta 2)]),(Respuesta 3),(Respuesta 4),(Respuesta 5),(Opciones [(Respuesta 6)])]

mapAD f (Respuesta x) = Respuesta (f x)
mapAD f x@(Opciones [])= x
mapAD f (Opciones (x:xs)) = merge (mapAD f x) (mapAD f (Opciones xs))
       where merge x (Opciones xs) = Opciones (x:xs)

prof y (Respuesta x) = Respuesta (x,y+1)
prof y (Opciones []) = Opciones []
prof y (Opciones (x:xs)) = merge (prof (y+1) x) (prof y (Opciones xs))
    where merge x (Opciones xs) = Opciones (x:xs)

profundidad xs = prof 0 xs

retprof' (Respuesta (x,y)) = [y]
retprof' (Opciones []) = []
retprof' (Opciones (x:xs)) = (retprof' x)++(retprof' (Opciones xs))

retprof xs = retprof' (profundidad xs)

buscar (Respuesta x) = Just x
buscar (Opciones (x:[])) = buscar x
buscar (Opciones (x:y:xs)) | retprof x > retprof y = buscar (Opciones (y:xs))
                           | otherwise = buscar (Opciones (x:xs))