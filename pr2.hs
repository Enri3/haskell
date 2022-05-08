--ejercicio 1
{-
data Color = Color{
                    rojo :: Int,
                    verde :: Int,
                    azul :: Int
                  } deriving(Show)

color1  = Color {
                  rojo = 35,
                  verde = 23,
                  azul = 56
}

color2  = Color {
                  rojo = 12,
                  verde = 63,
                  azul = 81
}

mezclar x y = Color { 
                      rojo = div (rojo x + rojo y) 2,
                      verde = div (verde x + verde y) 2,
                      azul = div (azul x + azul y) 2
                    }
-}
--ejercicio 2

data Linea = Linea {
                    caracteres :: [Char],
                    cursor :: Int
                   } deriving(Show)

linea1 = Linea {
                 caracteres = "",
                 cursor = 0
}

linea2 = Linea {
                 caracteres = "Hola",
                 cursor = 2
}

vacia x = caracteres x == ""

moverIzq x = if vacia x == False && cursor x /= 0 then x{caracteres = caracteres x, cursor = cursor x - 1} else x

moverDer x = if vacia x == False && cursor x < length (caracteres x) then x{caracteres = caracteres x, cursor = cursor x + 1} else x

moverIni x = x{caracteres = caracteres x, cursor = 0}

moverFin x = x{caracteres = caracteres x, cursor = length(caracteres x)}

insaux 0 c xs = c:xs
insaux i c (x:xs) = x:insaux (i-1) c xs

insertar c x = x{caracteres = insaux (cursor x) c (caracteres x), cursor = cursor x}

boraux 0 (x:xs) = xs
boraux i (x:xs) = x:boraux (i-1) xs

borrar x = x{caracteres = boraux (cursor x) (caracteres x), cursor = cursor x}

--ejercicio 3

clist1 = Consnoc 12 EmptyCL 2
clist2 = Consnoc 12 (Consnoc 3 EmptyCL 7) 2
clist3 = Consnoc 12 (Consnoc 3 (Consnoc 67 (CUnit 45) 32) 7) 2
clist4 = Consnoc clist1 (Consnoc clist1 EmptyCL clist1) clist1

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving(Show)

headCL:: CList a -> a
headCL (CUnit a) = a
headCL (Consnoc a xs b) = a

tailCL:: CList a -> CList a
tailCL (CUnit a) = EmptyCL
tailCL (Consnoc a EmptyCL b) = CUnit b
tailCL (Consnoc a xs b) = Consnoc (headCL xs) (tailCL xs) b

isEmptyCL:: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit:: CList a -> Bool
isCUnit (CUnit a) = True
isCUnit _ = False

reverseCL:: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit a) = CUnit a
reverseCL (Consnoc a xs b) = Consnoc b (reverseCL xs) a

cons EmptyCL a = CUnit a
cons (CUnit a) b = Consnoc b EmptyCL a
cons (Consnoc a xs b) c = Consnoc c (cons xs a) b

snoc EmptyCL a = CUnit a
snoc (CUnit a) b = Consnoc a EmptyCL b
snoc (Consnoc a xs b) c = Consnoc a (snoc xs b) c

sacault:: CList a -> CList a
sacault (CUnit a) = EmptyCL
sacault (Consnoc a EmptyCL b) = CUnit a
sacault (Consnoc a xs b) = Consnoc a (reverseCL(tailCL(reverseCL xs))) (headCL(reverseCL xs))

inits:: CList a -> CList(CList a)
inits' EmptyCL = EmptyCL
inits' (CUnit x) = CUnit (CUnit x)
inits' xs = snoc (inits'(sacault xs)) xs

inits xs = cons (inits' xs) EmptyCL

sacaprim:: CList a -> CList a
sacaprim (CUnit a) = EmptyCL
sacaprim (Consnoc a EmptyCL b) = CUnit b
sacaprim (Consnoc a xs b) = Consnoc (headCL xs) (tailCL xs) b

lasts:: CList a -> CList(CList a)
lasts' EmptyCL = EmptyCL
lasts' (CUnit x) = CUnit (CUnit x)
lasts' xs = cons (lasts'(sacaprim xs)) xs

lasts xs = snoc (lasts' xs) EmptyCL

snoc':: CList a -> CList a -> CList a
snoc' (CUnit a) EmptyCL = CUnit a
snoc' EmptyCL (CUnit a) = CUnit a
snoc' (Consnoc a xs b) EmptyCL = (Consnoc a xs b)
snoc' EmptyCL (Consnoc a xs b) = (Consnoc a xs b)
snoc' (CUnit a) (CUnit b) = Consnoc a EmptyCL b
snoc' (Consnoc a xs b) (CUnit x) = Consnoc a (snoc xs b) x
snoc' (CUnit x) (Consnoc a xs b) = Consnoc x (cons xs a) b
snoc' (Consnoc a xs b) (Consnoc x xy y) = Consnoc a (snoc' (snoc xs b) (cons xy x)) y

concatCL:: CList(CList a) -> CList a
concatCL EmptyCL = EmptyCL
concatCL (CUnit(CUnit a)) = CUnit a
concatCL xs = snoc' (headCL xs) (concatCL (tailCL xs))

--ejercicio 4

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

aexp1 = Prod (Prod (Num 3) (Num 4)) (Prod (Num 5) (Num 6))

eval:: Aexp -> Int
eval (Num a) = a
eval (Prod a b) = (eval a) * (eval b)
eval (Div a b) = div (eval a) (eval b)

--ejercicio 5

data BTS a = H | N (BTS a) a (BTS a) deriving(Show)

bts1 = N (N (N H 2 H) 5 (N H 9 H)) 10 (N (N (N (N H 11 H) 11 H) 11 H) 12 (N H 41 H))
bts2 = N (N H 5 (N (N H 3 H) 6 H)) 10 (N H 15 (N (N H 13 H) 16 H))
arbolNoBTS = N (N H 4 (N H 8 H)) 6 H

maximun:: BTS a -> a
maximun (N l a H) = a
maximun (N l a r) = maximun r

minimun:: BTS a -> a
minimun (N H a r) = a
minimun (N l a r) = minimun r

checkIzq a t@(N l x r) = if a > x then checkBTS t else False

checkDer a t@(N l x r) = if a < x then checkBTS t else False

checkBTS:: (Ord a) => BTS a -> Bool
checkBTS H = True
checkBTS (N H a H) = True
checkBTS (N H a r) = checkDer a r && a > minimun r && a < maximun r
checkBTS (N l a H) = checkIzq a l && a > minimun l && a < maximun l
checkBTS (N l a r) = (checkIzq a l) && (checkDer a r) && (a > minimun l) && (a < maximun r)

--ejercicio 6

completo:: a -> Int -> BTS a
completo a 0 = H
completo a x = N (completo a (x-1)) a (completo a (x-1))

balanceado:: a -> Int -> BTS a
balanceado a 0 = H
balanceado a x = N (balanceado a (div x 2)) a (completo a (div (x-1) 2))
{-
balanceado a 0 = H
balanceado a x = if (mod x 2) == 0 then N (balanceado (div x 2)) a (balanceado (div x 2)) else N (balanceado (div (x - 1) 2)) a (balanceado (div (x - 1) 2))
-}
--ejercicio 7
--KPOPER FAN BTS-DYNAMITE AMIGAAAAAAAAAAA

member:: (Ord a,Eq a) => a -> BTS a -> Bool
member x H = False
member x (N l y r) | x == y = True
                   | x < y = member x l
                   | otherwise = member x r

member2:: (Ord a,Eq a)=> a -> a -> BTS a -> Bool
member2 x c H = x == c
member2 x c (N l y r) | x > y = member2 x c r
                      | otherwise = member2 x y l

{-meber en funcion de member2

member:: (Ord a,Eq a)=> a -> BTS a -> Bool
member x H = False
member x t@(N l y r) = member2 x y t-}

--ejercicio 8

data Color = R | B deriving(Show)
data RBT a = E | T Color (RBT a) a (RBT a) deriving(Show)

makeBlack E = E
makeBlack (T _ l x r) = T B l x r

balance :: Color -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

insert:: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack(ins x t)
   where ins x E = T R E x E
         ins x (T c l y r) | x<y = balance c (ins x l) y r
                           | x>y = balance c l y (ins x r)
                           | otherwise = T c l y r

fromOrdList:: [a] -> RBT a
fromOrdList [] = E
fromOrdList (x:xs) = T R E x E
fromOrdList (x:y:xs) = T B (fromOrdList [x]) y (fromOrdList xs)