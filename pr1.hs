main = putStrLn "Hello, World!"

list1:: [Int]
list1 = [1,2,3,4,5]
list2 = [5,4,3,2,1]
list3 = [list1,list1,list1,list1]
list4 = [(2,3),(5,1),(2,6),(7,2),(87,3),(43,2),(6,4)]
list5 = ['a','w','f','g','a','e','g','#','3']
list6 = [True,True,True,True,True,False,False,False,False]
list7 = [True,True,True,True,True]
list8 = [False,False,False,False]

--ejercicio 0
data Nat = Zero | Succ Nat

add n Zero = n
add n (Succ m) = Succ (add m n)

nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

natmult n Zero = Zero
natmult n (Succ m) = add n (natmult n m)

natpot n Zero = Succ(Zero)
natpot n (Succ m) = natmult n (natpot n m)

--ejercicio 1

regla b = case b of
           True -> "Quedate en Casa"
           False -> "Qudate en Casa"

cases []          =  []
cases [x]         =  []
cases (x:y:xs)    =  y : cases (x:xs)

--me canse sigo con el 2

--ejercicio 2

five:: a' -> Int
five x = 5

apply:: (a' -> a') -> a' -> a'
apply fun x = fun x

identidad:: a' -> a'
identidad x = x

first:: (a,b) -> a
first (a,b) = a

--por razones a omitidas no se raliza el e)

sign:: Int -> Bool
sign x = x>=0

vabs:: Int -> Int
vabs x = if sign x then x else x * (-1)

vabs2:: Int -> Int
vabs2 x = if x>=0 then x else x * (-1)

pot:: Int -> Int -> Int
pot x 0 = 1
pot x y = x * pot x (y-1)

xor:: Bool -> Bool -> Bool
xor x y = x || y

max3:: Int -> Int -> Int -> Int
max3 x y z = if x>y then
              if x>z then x else
                if y>z then y else z
                  else if y>z then y else z

swap:: (a,b) -> (b,a)
swap (x,y) = (y,x)

--ejercicio 3

esBisiesto:: Int -> Bool
esBisiesto x = (mod x 4 == 0) && (mod x 100 /= 0) || (mod x 400 == 0)

--ejercicio 4

fun1:: (Int -> Int) -> Int
fun1 fun = fun 1

fun2:: (Int -> Int) -> Int
fun2 fun = fun 0

fun3:: Int -> (Int -> Int)
fun3 x y = x * y

fun4:: Int -> (Int -> Int)
fun4 x y = x + y

fun5:: (Int -> Int) -> (Int -> Int)
fun5 identidad = vabs

fun6:: (Int -> Int) -> (Int -> Int)
fun6 vabs = identidad

fun7:: Int -> Bool
fun7 x = True

fun8:: Int -> Bool
fun8 x = False

fun9:: Bool -> (Bool -> Bool)
fun9 x = fun9 True

fun10:: Bool -> (Bool -> Bool)
fun10 x = fun9 False

fun11:: (Int,Char) -> Bool
fun11 (x,y) = x > 5

fun12:: (Int,Char) -> Bool
fun12 (x,y) = x < 5

fun13:: (Int,Int) -> Int
fun13 (x,y) = x

fun14:: (Int,Int) -> Int
fun14 (x,y) = y

fun15:: Int -> (Int,Int)
fun15 x = (x,x * x)

fun16:: Int -> (Int,Int)
fun16 x = (x,x + x)

fun17:: a -> Bool
fun17 x = True

fun18:: a -> Bool
fun18 x =  False

fun19:: a -> a
fun19 x = x

fun20:: a -> a
fun20 x = x

--ejercicio 5

divisors x = [a | a <- [1..x], mod x a == 0]

matches x xs = [a | a <- xs, a == x]

isin a [] = False
isin a (x:xs) = if a == x then True else isin a (xs)

elim 0 xs = xs
elim x xs = elim (x-1) (tail(xs))

duplador _ [] = []
duplador [] _ = []
duplador (x:xs) (y:ys) = ((x,y):duplador xs ys)

unique xs = [a | (a,i) <- duplador xs [1..], not(isin a (elim i xs))]

--ejercicio 6

producto [] = []
producto ((x,y):xs) = ((x * y):producto xs) 

scalarProduct xs ys = sum(producto(zip xs ys))

--ejercicio 7

suma [] = 0
suma (x:xs) = suma xs + x

alguno:: [Bool] -> Bool
alguno [] = False
alguno (x:xs) = if x == True then True else alguno xs

todos:: [Bool] -> Bool
todos [] = False
todos (x:xs) = if x /= True then False else todos xs

codes:: [Char] -> [Int]
codes [] = []
codes (x:xs) = 1:codes xs

restos:: [Int] -> Int -> [Int]
restos [] _ = []
restos (x:xs) a = x `rem` a:restos xs a

cuadrados:: [Int] -> [Int]
cuadrados [] = []
cuadrados (x:xs) = x * x:cuadrados xs

long [] = 0
long (x:xs) = 1 + long xs

longitudes:: [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = long x:longitudes xs

orden:: [(Int,Int)] -> [(Int,Int)]
orden [] = []
orden ((a,b):xs) = if a < (b * 3) then (a,b):orden xs else orden xs

pares:: [Int] -> [Int]
pares [] = []
pares (x:xs) = if x `rem` 2 == 0 then x:pares xs else pares xs

letras:: [Char] -> [Char]
letras [] = []
letras (x:xs) = if x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z' then x:letras xs else letras xs

filtro [] n = []
filtro ((a,b):xs) n = if b > n then a:filtro xs n else filtro xs n

masDe:: [[a]] -> Int -> [[a]]
masDe xss n = filtro (duplador xss (longitudes xss)) n

--ejercicio 8

sumame x y = x + y
suma2 xs = foldl sumame 0 xs

alguno2 xs = foldl (||) False xs

todos2 xs = foldl (&&) True xs

--codes2 no se no entiendo

{-resto x = x `rem` n
restos2 xs n = map resto xs-}

--quien mierda hizo estos ejercicios
--no entiendo nada me voy a matar
--si alguien los hizo me los pasa uwu