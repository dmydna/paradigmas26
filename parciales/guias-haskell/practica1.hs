import Prelude hiding (Maybe, Either, map, foldr, foldl, elem, sum, (++))
import Data.List hiding (map, foldr, foldl, elem,(++),sum)
import Data.Maybe


-- :: esquemas de recursion ::

-- estructural
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x : xs) = f x (foldr f acc xs)

-- primitiva
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

-- iterativa
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f rec [] = rec
foldl f rec (x : xs) = foldl f (f rec x) xs



-- Ejercicio 1

--max2 :: Ord a => (a,a) -> a
--max2Curry :: Ord a => a -> a -> (a -> a -> b) 
--normaVectorial :: Floating a => (a, a) -> a
--normaVectorialCurry :: Floating a => a -> a -> (a -> a -> b)



-- Ejercicio 2

curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x,y)

uncurry :: (a->b->c) -> (a,b)-> c
uncurry f = \(x, y) -> f x y


-- Ejercicio 3 ★

sum :: Num a => [a] -> a
sum = foldr (+) 0

elem :: Eq a => a -> [a] -> Bool
elem n = foldr (\x rec -> x == n || rec) False
--elem n = foldr (\x rec -> if x == n then True  else rec) False

(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (:) xs ys

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x rec ->  if p x then x : rec else rec)[]

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x rec -> f x : rec) []

mejorSegún :: (a -> a -> Bool) -> [a] -> a
mejorSegún f = foldr1 (\x rec -> if f x rec then x else rec)

sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\rec x -> if null rec then [x] else rec ++ [x + last rec]) []

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

sumaAlt_inv :: Num a => [a] -> a
sumaAlt_inv = foldl (-) 0

-- Ejercicio 4 

--permutaciones :: [a] -> [[a]]
--partes
--prefijos
--sublistas

-- Ejercicio 5 (teorico)

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                                    then [x]
                                    else x : elementosEnPosicionesPares (tail xs)


entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id 
entrelazar (x:xs) = \ys -> if null ys 
                           then x : entrelazar xs []
                           else x : head ys : entrelazar xs (tail ys)


-- Ejercicio 6 ★

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna n = recr (\x xs rec -> if x == n then xs else x : rec) []


insertarOrdenado :: Ord a => a -> [a] -> [a] 
insertarOrdenado n = recr (\x xs rec -> if n <= x then n : x : xs  else x : rec) []

-- Ejercicio 7 ★

-- mapPares

-- aramarPares


-- Ejercicio 9 ★

data Nat = Zero | Succ Nat

foldNat :: (b -> b) -> b -> Nat -> b
foldNat  cSucc cZero  t = case t of
            Zero   -> cZero 
            Succ n -> cSucc (rec n)
            where rec = foldNat cSucc cZero

potenciaNat :: Int -> Nat -> Int 
potenciaNat base exp = foldNat(\rec -> rec * base) 1 exp 


-- Ejercicio 10

genLista :: a -> (a->a) -> Integer -> [a]
genLista start incr  size = take (fromInteger size) (iterate incr start)

desdeHasta :: Num a => a -> Integer -> [a]
desdeHasta start end  =  genLista start (+1) end 


-- Ejercicio 11

data Polinomio a = X
                 | Cte a 
                 | Suma (Polinomio a) (Polinomio a)
                 | Prod (Polinomio a) (Polinomio a)


foldPoli :: (b -> b ->b) -> (b -> b ->b) -> (a -> b) -> b -> Polinomio a -> b
foldPoli cProd cSuma cCte cX t = case t of
                   X -> cX 
               Cte a -> cCte a
          Suma r1 r2 -> cSuma (rec r1) (rec r2)
          Prod r1 r2 -> cProd (rec r1) (rec r2)
          where rec = foldPoli cProd cSuma cCte cX

evaluar :: Num a => a -> Polinomio a -> a
evaluar n = foldPoli (*) (+) (id) n

-- Ejercicio 12 ★

data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: (b->a->b) -> b -> AB a -> b
foldAB cBin cNil t = case t of
                  Nil -> cNil
            Bin i r d -> cBin (rec i) r (rec d) 
            where rec = foldAB cBin cNil 

recAB :: (b->a->b) -> b -> AB a -> b
recAB cBin cNil t = case t of
                  Nil -> cNil
            Bin i r d -> cBin r i d (rec i) (rec d) 
            where rec = foldAB cBin cNil 

esNil :: AB a -> Bool
esNil Nil = True
esNil _   = False

altura :: AB a -> Int 
altura = foldAB (\ri r rd ->  1 + max ri rd) 0

cantNodos :: AB a -> Int
cantNodos = foldAB (\ri r rd ->  1 + ri + rd) 0


mejorSegun :: (a->a->Bool) -> AB a -> a
mejorSegun pred Nil         = Nil
mejorSegun pred (Bin  i  r  d) = case (rec1, rec2) of
                (Nil, Nil) -> r
                (ri , Nil) -> if pred r ri then r else ri
                (Nil,  rd) -> if pred r rd then r else rd
                (ri ,  rd) -> if pred r p  then r else p
                where rec1 = mejorSegun pred i
                      rec2 = mejorSegun pred d
                         p = if pred ri rd then ri else rd

esABB :: Ord a => AB a -> Bool
esABB = foldAB (\reci r recd ->  r > reci && r < recd) True


































