--cs.us.es/~jalonso/cursos/i1m/
--cs.us.es/~jalonso/cursos/i1m/temas.php

--cs.us.es/~jalonso/cursos/i1m-18/temas/tema-1.html

--cs.us.es/~jalonso/cursos/i1m-18/temas/tema-5.html
--youtube.com/watch?v=7_8R7WFpzbM
--youtube.com/watch?v=KXNqVQ-g-zo


import Data.Char
--import Test.QuickCheck
--import Test.QuickCheck.Function
--import CodeWorld

-- <:set +s> para activar contador de recursos usados
-- <:unset +s> para desactivar
-- <mapM_ print> para displayear lista "hacia abajo"
-- cse.chalmers.se/~rjmh/QuickCheck/


--  ===================================================================================
--  *  *  *  *  *  * TEMA 1. INTRODUCCION A LA PROGRAMACION FUNCIONAL  *  *  *  *  *  *
--  ===================================================================================

doble3 :: Num a => a -> a
doble3 x = x + x

cuadruple :: Num a => a -> a
cuadruple = doble3 . doble3

factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

factorial2 :: Int -> Int
factorial2 n = product [1..n]

factorial3 :: Integer -> Integer    -- <<--supuestamente convendría ésta
factorial3 n = product [1..n]


--  ===================================================================================
--  *  *  *  *  *  TEMA 2. INTRODUCCION A LA PROGRAMACION CON HASKELL  *  *  *  *  *  *
--  ===================================================================================

ordena :: Ord a => [a] -> [a]
ordena [] = []
ordena (x:xs) =
    (ordena menores) ++ [x] ++ (ordena mayores)
    where
        menores = [a | a <- xs, a <= x]
        mayores = [b | b <- xs, b > x]


--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  *  TEMA 3. TIPOS Y CLASES  *  *  *  *  *  *  *  *  *  *
--  ===================================================================================

sumaI :: Int -> Int -> Int
sumaI x y = x + y

suc :: Int -> Int
suc = sumaI 1


--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  TEMA 4. DEFINICION DE FUNCIONES  *  *  *  *  *  *  *  *
--  ===================================================================================

esDigito :: Char -> Bool
esDigito c = c >= '0' && c <= '9'

esPar :: Int -> Bool
esPar n = n `rem` 2 == 0

divLista :: Int -> [a] -> ([a], [a])
divLista n xs = (take n xs, drop n xs)

valAbsoluto :: (Num a, Ord a) => a -> a
valAbsoluto n = if n >= 0 then n else -n

valAbsoluto' :: (Num a, Ord a) => a -> a
valAbsoluto' n
    | n >= 0 = n
    | otherwise = -n

signo :: (Num a, Ord a) => a -> a
signo n = if n < 0 then (-1) else
          if n == 0 then 0 else 1

signo' :: (Num a, Ord a) => a -> a
signo' n
    | n < 0 = (-1)
    | n == 0 = 0
    | otherwise = 1

segundo :: (a, b) -> b
segundo (_, x) = x

tercero :: (a, b, c) -> c
tercero (_, _, x) = x

prueba1 :: [Char] -> Bool  --vale también con <String>
prueba1 ['a', _, _] = True
prueba1 _ = False

empiezaConH :: String -> Bool
empiezaConH ('h' : _) = True
empiezaConH _ = False

cabeza :: [a] -> a
cabeza [] = error "Ya estamos con las listas vacías"
cabeza (x:_) = x

--sin lambda
doble' :: Num a => a -> a
doble' x = x + x

--con lambda
doble :: Num a => a -> a
doble = \x -> x + x  -- <<---la función (lambda) que a un <x> le asigna <x + x>

--sin lambda
suma' :: Num a => a -> a -> a
suma' x y = x + y

--con lambda
suma :: Num a => a -> a -> a
suma = \x y -> x + y
--suma = \x -> (\y -> x + y)  <<---otra manera, una adentro de otra

--sin lambda
constante' :: a -> b -> a
constante' x y = x

--con lambda
constante :: a -> b -> a
constante x = \_ -> x

--sin lambda
impares' :: (Num b, Enum b) => b -> [b]
impares' n = map f [0..n-1]
    where f x = 2 * x + 1

--con lambda
impares :: (Num b, Enum b) => b -> [b]
impares n = map (\x -> 2 * x + 1) [0..n-1]


--  ===================================================================================
--  *  *  *  *  *  *  *  *  * TEMA 5. LISTAS POR COMPRENSION  *  *  *  *  *  *  *  *  *
--  ===================================================================================


concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

primeros :: [(a, b)] -> [a]
primeros ps = [x | (x,_) <- ps]

longitud :: [a] -> Int
longitud xs = sum [1 | x <- xs]

factores :: Int -> [Int]
factores x = [y | y <- [1..x], rem x y == 0]

primo :: Int -> Bool
primo x = factores x == [1, x]

primos :: Int -> [Int]
primos x = [y | y <- [2..x], primo y]

busca :: Eq a => a -> [(a, b)] -> [b]
busca clave listaPares = [valor | (clave', valor) <- listaPares, clave' == clave]
--busca c t = [v | (c', v) <- t, c' == c]

adyacentes :: [a] -> [(a, a)]
adyacentes xs = zip xs (tail xs)

ordenada :: Ord a => [a] -> Bool
ordenada xs = and [ x <= y | (x, y) <- adyacentes xs]

posiciones :: Eq a => a -> [a] -> [Int]
posiciones x ys = busca x (zip ys [0..])

posiciones' :: Eq a => a -> [a] -> [Int]
posiciones' x xs =
    [i | (x', i) <- zip xs [0..n], x == x']
    where n = length xs - 1

posiciones'' :: Eq a => a -> [a] -> [Int]
posiciones'' x xs = [i | (x', i) <- zip xs [0..], x == x']

minusculas' :: String -> String
minusculas' xs = [x | x <- xs, elem x ['a'..'z']]

ocurrencias :: Char -> String -> Int
ocurrencias x xs = length [x' | x' <- xs, x == x']

ocurrencias' :: Char -> String -> Int
ocurrencias' x xs = length (posiciones x xs)

-- ----------------------------------------
-- CIFRADO CESAR                         --
-- ----------------------------------------

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

desplaza :: Int -> Char -> Char
desplaza n c
    | isLower c = int2let ((let2int c + n) `mod` 26)
--  | elem c ['a'..'z'] = int2let ((let2int c + n) `mod` 26)  <<---otra forma
    | otherwise = c

prop_desplaza n x = desplaza (-n) (desplaza n x) == x  --para usar con quickCheck

codifica :: Int -> String -> String
codifica n xs = [desplaza n x | x <- xs]

prop_codifica n xs = codifica (-n) (codifica n xs) == xs  --para usar con quickCheck

tabla :: [Float]
tabla = [12.53, 1.42, 4.68, 5.86, 13.68, 0.69, 1.01, 
          0.70, 6.25, 0.44, 0.01,  4.97, 3.15, 6.71, 
          8.68, 2.51, 0.88, 6.87,  7.98, 4.63, 3.93, 
          0.90, 0.02, 0.22, 0.90,  0.52]

minusculas :: String -> String
minusculas xs = [x | x <- xs, isLower x]

porcentaje :: Int -> Int -> Float
porcentaje n m = (fromIntegral n / fromIntegral m) * 100

frecuencias :: String -> [Float]
frecuencias xs =
    [porcentaje (ocurrencias x xs) n | x <- ['a'..'z']]
    where n = length (minusculas xs)

chiCuadrado :: [Float] -> [Float] -> Float
chiCuadrado os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]
--os = numero observado
--es = numero esperado

rotaLista :: Int -> [a] -> [a]
rotaLista n xs = drop n xs ++ take n xs

descodifica :: String -> String
descodifica xs = codifica (-factor) xs
    where
        factor = head (posiciones (minimum tabChi) tabChi)
        tabChi = [chiCuadrado (rotaLista n tabla') tabla | n <- [0..25]]
        tabla' = frecuencias xs

{-
   descifra :: String -> String
   descifra xs =  codifica (-factor) xs
   where
     factor = head (posiciones (minimum tabChi) tabChi)
     tabChi = [chiCuadrado (rotaLista n tabla') tabla | n <- [0..25]]
     tabla' = frecuencias xs
-}


--  ===================================================================================
--  *  *  *  *  *  *  *  *  TEMA 7. FUNCIONES DE ORDEN SUPERIOR  *  *  *  *  *  *  *  *
--  ===================================================================================


dosVeces :: (a -> a) -> a -> a
dosVeces f x = f (f x)

--por comprensión
mapeo :: (a -> b) -> [a] -> [b]
mapeo f xs = [f x | x <- xs]

--por recursión
mapeo' :: (a -> b) -> [a] -> [b]
mapeo' _ [] = [] --error "Ya estamos..." <<---esto no va por el ending de la func
mapeo' f (x:xs) = f x : mapeo' f xs

--por recursión
sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

--por comprensión
filtra :: (a -> Bool) -> [a] -> [a]
filtra propiedad xs = [x | x <- xs, propiedad x]  --la lista equises, tal que x pertenece
                                                  --a xs, y x cumple con la <propiedad>

--por recursión
filtra' :: (a -> Bool) -> [a] -> [a]
filtra' _ [] = [] --error "Ya estamos..." <<---esto no va por el ending de la func
filtra' propiedad (x:xs)
    | propiedad x = x : filtra' propiedad xs
    | otherwise = filtra' propiedad xs

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares xs = sum (filter even (map (^2) xs))

sumaCuadradosPares' :: [Int] -> Int
sumaCuadradosPares' xs = sum [x^2 | x <- xs, even x]

sumaCuadradosPares'' :: [Int] -> Int
sumaCuadradosPares'' xs = sum (map (^2) (filter even xs))

todos :: (a -> Bool) -> [a] -> Bool
todos _ [] = True
todos p (x:xs) = p x && todos p xs

alguno :: (a -> Bool) -> [a] -> Bool
alguno _ [] = False
alguno p (x:xs) = p x || alguno p xs

tomarMientras :: (a -> Bool) -> [a] -> [a]
tomarMientras _ [] = []
tomarMientras p (x:xs)
    | p x = x : tomarMientras p xs
    | otherwise = []


borrarMientras :: (a -> Bool) -> [a] -> [a]  --está mal de todas formas
borrarMientras _ [] = []
-- xs es un área de la lista que tiene el primer elemento x iy el resto xs prima
borrarMientras p xs@(x:xs')
    | p x = borrarMientras p xs'
    | otherwise = xs

sumLista :: Num a => [a] -> a
sumLista [] = 0
sumLista (x:xs) = x + sumLista xs

productLista :: Num a => [a] -> a
productLista [] = 1
productLista (x:xs) = x * productLista xs

oOo :: [Bool] -> Bool
oOo [] = False
oOo (x:xs) = x || oOo xs

yYy :: [Bool] -> Bool
yYy [] = True
yYy (x:xs) = x && yYy xs

-- Esquema básico de recursión sobre listas
-- f [] = elemNeutro
-- f (x:xs) = x `operación` (f xs)


-- ----------------------------
-- Definiciones con patrón FODL
-- ----------------------------


sumFoldr xs = foldr (+) 0 xs -- sumFoldr = foldr (+) 0 ,, como es, pero no quiere ghci
productFoldr xs = foldr (*) 1 xs
orFoldr xs = foldr (||) False xs
andFoldr xs = foldr (&&) True xs


longRecurs :: [a] -> Int
longRecurs [] = 0
longRecurs (_:xs) = 1 + longRecurs xs

{-
   longFoldr [4,8,5]
   = 4 `f` 8 `f` 5 `f` e
   = 4 `f` (8 `f` (5 `f` e))
   = 4 `f` (8 `f` (5 `f` 0))   -- e     = 0
   = 4 `f` (8 `f` 1)           -- f 5 0 = 1
   = 4 `f` 2                   -- f 8 1 = 2
   = 3                         -- f 4 2 = 3
                               -- f x n = n + 1
-}

longFoldr :: [a] -> Int
longFoldr = foldr f 0
    where f x n = n + 1

longFoldr2 :: [a] -> Int
longFoldr2 = foldr (\x n -> n + 1) 0

longFoldr3 :: [a] -> Int
longFoldr3 = foldr (\_ n -> n + 1) 0

inversaRecurs :: [a] -> [a]
inversaRecurs [] = []
inversaRecurs (x:xs) = inversaRecurs xs ++ [x]

{-
   inversa [4,8,5]
   = 4 `f` 8 `f` 5 `f` e
   = 4 `f` (8 `f` (5 `f` e))
   = 4 `f` (8 `f` (5 `f` []))   -- e         = []
   = 4 `f` (8 `f` [5])          -- f 5 []    = [5]
   = 4 `f` [5,8]                -- f 8 [5]   = [5,8]
   = [5,8,4]                    -- f 4 [5,8] = [5,8,4]
                                -- f x ys    = ys ++ [x]
-}

inversaFoldr :: [a] -> [a]
inversaFoldr = foldr f []
    where f x ys = ys ++ [x]

inversaFoldr2 :: [a] -> [a]
inversaFoldr2 = foldr (\x ys -> ys ++ [x]) []

concatRecurs :: [a] -> [a] -> [a]
concatRecurs [] ys = ys
concatRecurs (x:xs) ys = x : concatRecurs xs ys

{-
   conc [3,2,5] [4,7]
   = 3 `f` 2 `f` 5 `f` e
   = 3 `f` (2 `f` (5 `f` e))
   = 3 `f` (2 `f` (5 `f` [4,7]))   -- e             =       [4,7]
   = 3 `f` (2 `f` [5,4,7])         -- f 5 [4,7]     =     [5,4,7]
   = 3 `f` [2,5,4,7]               -- f 2 [5,4,7]   =   [2,5,4,7]
   = [3,2,5,4,7]                   -- f 3 [2,5,4,7] = [3,2,5,4,7]
                                   -- f x ys = x : ys
-}

concatFoldr :: [a] -> [a] -> [a]
concatFoldr xs ys = (foldr f ys) xs
    where f x ys = x : ys

concatFoldr2 :: [a] -> [a] -> [a]
concatFoldr2 xs ys = (foldr f ys) xs
    where f x ys = (:) x ys             -- f(x) = cos(x)
                                        -- f    = cos

concatFoldr3 :: [a] -> [a] -> [a]
concatFoldr3 xs ys = (foldr f ys) xs
    where f = (:)

concatFoldr4 :: [a] -> [a] -> [a]
concatFoldr4 xs ys = (foldr (:) ys) xs

concatFoldr5 :: [a] -> [a] -> [a]
concatFoldr5 xs ys = foldr (:) ys xs

concatFoldl :: [a] -> [a] -> [a]
concatFoldl xs ys = undefined

--por plegado, suma
{-
   suma [3,4,5]

   0 [3,4,5]
   3 [4,5]
   7 [5]
  12 []
-}

suma3 :: Num a => [a] -> a
suma3 = sumaAux 0
    where sumaAux v [] = v
          sumaAux v (x:xs) = sumaAux (v + x) xs

sumaAux v [] = v
sumaAux v (x:xs) = sumaAux (v + x) xs


sumFoldl xs     = foldl (+) 0 xs -- sumFoldr = foldr (+) 0 ,, como es, pero no quiere ghci
productFoldl xs = foldl (*) 1 xs
orFoldl xs      = foldl (||) False xs
andFoldl xs     = foldl (&&) True xs

defineFoldl :: (a -> b -> a) -> a -> [b] -> a
defineFoldl f v [] = v
defineFoldl f v (x:xs) = foldl f (f v x) xs

-- ------------------------
-- Composición de funciones
-- ------------------------

composicion :: (b -> c) -> (a -> b) -> a -> c
composicion f g x = f (g x)
--según haskell >>  (.) f g = \x -> f (g x)

composicion2 :: (b -> c) -> (a -> b) -> a -> c
composicion2 f g = h
    where h x = f (g x)

composicion3 :: (b -> c) -> (a -> b) -> a -> c
composicion3 f g = h
    where h = \x -> f (g x)
        --h es una funión que toma como argumento un elemento x, y me devuelve f de g x

composicion4 :: (b -> c) -> (a -> b) -> a -> c
composicion4 f g = \x -> f (g x)

impar = odd

par :: Int -> Bool
par n = not (impar n)
par2 n = (not . impar) n
par3 = not . impar   --la función par es la negación compuesta con impar

--dosVeces :: (a -> a) -> a -> a
--dosVeces f x = f (f x)
dosVeces2 f x = (f . f) x   --f compuesto con f, aplicado a x
dosVeces3 f = f . f

--sumaCuadradosPares :: [Int] -> Int
--sumaCuadradosPares xs = sum (filter even (map (^2) xs))
sumaCuadradosPares2 xs = (sum . map (^2) . filter even) xs
sumaCuadradosPares3 = sum . map (^2) . filter even
                    --la suma de los cuadrados de los pares
                    --quédate con los pares, eleva cada elemento al cuadrado, y súmalos
--por comprensión:
sumaCuadradosPares4 xs = sum [x ^2 | x <- xs, even x]

composicionLista :: [a -> a] -> (a -> a)   ---según yo >> :: [a -> a] -> a -> a
composicionLista [] x = x
composicionLista (f:fs) x = f (composicionLista fs x)

composicionLista2 [] x = id x
composicionLista2 (f:fs) x = (f . composicionLista2 fs) x

composicionLista3 [] = id
composicionLista3 (f:fs) = f . composicionLista3 fs

--con plegado (fold):
composicionLista4 fs = foldr (.) id fs


--  ===================================================================================
--  *  *  *  *  *  *  *  TEMA 9. DECLARACIONES DE TIPOS Y CLASES  *  *  *  *  *  *  *
--  ===================================================================================

type Posiscion = (Int, Int)

origen :: Posiscion
origen = (0, 0)

destino :: Posiscion
destino = (5, 8)

izquierda :: Posiscion -> Posiscion
izquierda (x,y) = (x - 1, y)

type Par a = (a, a)

ej1DePar :: Par Int
ej1DePar = (5, 8)

ej2DePar :: Par Bool
ej2DePar = (True, False)

type Par2 a b = (a, b)

ej3DePar :: Par2 Int Bool
ej3DePar = (5, False)

multiplica :: Num a => Par a -> a
multiplica (x, y) = x * y

unePar :: Par Char -> String
unePar (x, y) = [x, y]

intercambia :: Par a -> Par a
intercambia (x, y) = (y, x)

type Movimiento = Posiscion -> Posiscion

data Mov = Izquierda | Derecha | Arriba | Abajo
    deriving (Show, Eq)

movimiento :: Mov -> Posiscion -> Posiscion
movimiento Izquierda (x, y) = (x - 1, y) 
movimiento Derecha (x, y) = (x + 1, y)
movimiento Arriba (x, y) = (x, y + 1)
movimiento Abajo (x, y) = (x, y - 1)

movimientos :: [Mov] -> Posiscion -> Posiscion
movimientos [] (x, y) = (x, y)
movimientos (m:ms) (x, y) = movimientos ms (movimiento m (x, y))

opuesto :: Mov -> Mov
opuesto Izquierda = Derecha
opuesto Derecha = Izquierda
opuesto Arriba = Abajo
opuesto Abajo = Arriba

data Figura = Circulo Float | Rectangulo Float Float

cuadrado :: Float -> Figura
cuadrado n = Rectangulo n n

area :: Figura -> Float
area (Circulo r) = pi * r^2
area (Rectangulo b h) = b * h

-- data Maybe a = Nothing | Just a

divisionSegura :: Int -> Int -> Maybe Int
divisionSegura _ 0 = Nothing
divisionSegura x y = Just (div x y)

headSegura :: [a] -> Maybe a
headSegura [] = Nothing
headSegura (x:xs) = Just x

data Nat = Cero | Sig Nat
    deriving (Show, Eq)

nat2int :: Nat -> Int
nat2int Cero = 0
nat2int (Sig n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Cero
int2nat n = Sig (int2nat (n - 1))

sumaNat :: Nat -> Nat -> Nat
sumaNat Cero n = n
sumaNat (Sig m) n = Sig (sumaNat m n)

-- Nil y Cons son históricos, vienen de la época de Lisp
data Lista a = Nil | Cons a (Lista a)

longitud2 :: Lista a -> Int
longitud2 Nil = 0
longitud2 (Cons x xs) = 1 + longitud2 xs

data Arbol = Hoja Int | Nodo Arbol Int Arbol
    deriving (Show, Eq)

ejArbol :: Arbol
ejArbol = Nodo (Nodo (Hoja 1) 3 (Hoja 4))
               5
               (Nodo (Hoja 6) 7 (Hoja 9))

--       5
--      / \
--     /   \
--    /     \
--   3       7
--  / \     / \
-- 1   4   6   9

-- 1 3 4 5 6 7 9   <<---el árbol aplanado, o visto desde abajo

ocurre :: Int -> Arbol -> Bool
ocurre n (Hoja m) = n == m
ocurre n (Nodo i m d) = ocurre n i || n == m || ocurre n d

aplana :: Arbol -> [Int]
aplana (Hoja m) = [m]
aplana (Nodo i m d) = aplana i ++ [m] ++ aplana d

ocurreEnArbolOrdenado :: Int -> Arbol -> Bool
ocurreEnArbolOrdenado n (Hoja m) = n == m
ocurreEnArbolOrdenado n (Nodo i m d)
    | n == m = True
    | n < m = ocurreEnArbolOrdenado n i
    | otherwise = ocurreEnArbolOrdenado n d


--  ===================================================================================
--  *  *  *  *  *  *  *  *  *  TEMA 10. EVALUACION PEREZOSA  *  *  *  *  *  *  *  *  *
--  ===================================================================================


multiplicacion :: (Int, Int) -> Int
multiplicacion (x, y) = x * y

multiplicacion2:: Int -> Int -> Int
multiplicacion2 x = \y -> x * y    -- <<---una función que toma un argumento 'x' y devuelve
                                        -- una función que toma un argumento 'y' y devuelve
                                        -- 'x' * 'y'

infinito :: Int
infinito = 1 + infinito

cuadrado2 :: Int -> Int
cuadrado2 n = n * n

primos2 :: [Int]
primos2 = criba [2..]

criba :: [Int] -> [Int]
criba (p:xs) = p : criba [ x | x <- xs, x `mod` p /= 0]

sumaNE :: [Int] -> Int    -- <<---suma NoEstricta
sumaNE xs = sumaNE' 0 xs

sumaNE' :: Int -> [Int] -> Int
sumaNE' v [] = v
sumaNE' v (x:xs) = sumaNE' (v + x) xs

sumaE :: [Int] -> Int
sumaE xs = sumaE' 0 xs

sumaE' :: Int -> [Int] -> Int
sumaE' v [] = v
sumaE' v (x:xs) = (sumaE' $! (v + x)) xs