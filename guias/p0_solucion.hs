module Funcional where

-- EJERCICIO 1::..

{-
null: tipo nulo

head :: [a] -> a
devuelve el primer elemento de una secuencia

last :: [a] -> a
devuelve el ultimo elemento de una secuencia

tail :: [a] -> [b]
devuelve la lista sin el primer elemento

reverse :: [a] -> [a]
devuelve la lista en orden inverso cada elemento

elem :: [a] -> a -> Bool

concat :: [a] -> [b] -> [b]

drop :: 

-}
-- 
-- head: devuelve el primer elemento de una secuencia
-- tails: devuelve la secuencia sin su primer elemento 
-- init:
-- last:: [a] -> a
-- obtiene el ultimo elemento de una lista
-- drop:
-- (++): concatena dos secuencias
-- concat:

-- EJERCICIO 2::..

valorAbsoluto :: Float -> Float 
valorAbsoluto n | n <= 0 = n * (-1)
                | otherwise = n 


bisiesto :: Int -> Bool
bisiesto n | n `mod` 4 == 0 = True
           | n `mod` 100 == 0 = False
           | otherwise = False

factorial :: Int -> Int 
factorial 0 = 1
factorial n | otherwise = factorial n * n - 1


cantDivisores :: Int -> Int
cantDivisores 1 = 1
cantDivisores n | n `mod` n-1 == 0 = cantDivisores n-1 + 1
                | otherwise = cantDivisores n-1
 

esPrimo :: Int -> Bool
esPrimo n = cantDivisores n == 1


cantDivisoresPrimo :: Int -> Int 
cantDivisoresPrimo 1 = 1
cantDivisoresPrimo n | esPrimo n  && n `mod` n-1 = cantDivisoresPrimo n-1 + 1
                     | otherwise = cantDivisoresPrimo n-1 



-- EJERCICIO 3::..

data Maybe a = Nothing | Just a
data Either a b = Left a | Right b



inverso :: Float -> Maybe Float
inverso n | n == 0 = Nothing
          | otherwise = 1 / Just n



aEntero (Left a)  = a
aEntero :: Either Int Bool -> Int 
aEntero (Right b) | b == True = 1
                  | otherwise = 0

-- a es Int 
-- b es Bool
-- (Left a) es (Either Int Bool)
-- (Right b) es (Either Int Bool)



-- EJERCICIO 4::..


eliminarChar :: Char -> String -> String
eliminarChar a [] = []
eliminarChar a (x:xs) | a /= x  = x : eliminarChar a xs 
                      | otherwise = eliminarChar a xs

limpiar :: String -> String -> String 
limpiar [] b = b 
limpiar (x:xs) b = limpiar xs (eliminarChar x b) 


sum  :: [Float] -> Int
sum [] = 0
sum (x:xs) = sum xs + x

len :: [Float] -> Int
len [] = 0
len (x:xs) = len xs + 1


promedio :: [Float] -> [Float]
promedio s = sum s / len s 

difPromedioAux :: [Float] -> Float -> [Float]
difPromedioAux [] prom     = []
difPromedioAux (x:xs) prom | otherwise = x - prom : difPromedioAux xs


difPromedio :: [Float] -> [Float]
difPromedio s  = difPromedioAux s (promedio s)


todoIguales :: [Int] -> Bool 
todoIguales [x] = True 
todosIguales (x:y:xs) | x == y = todoIguales y:xs
                      | otherwise = False 




-- EJERCICIO 5::..

data AB a = Nil | Bin (AB a) a (AB a)



vacioAB :: AB a -> Bool
vacioAB AB a | a == Nil = True
             | otherwise = False 

negacionAB :: AB Bool → AB Bool
negacionAB AB a = Bin AB (not a)


productoAB :: AB Int → Int
productoAB AB a | a == True = Bin AB (1) 
                | otherwise = Bin AB (0)



