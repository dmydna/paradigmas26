import Prelude hiding (Maybe, Either)


-- Ejercicio 1 ::

{--
  null :: Foldable t => t a -> Bool
  Comprueba si la estructura está vacía

  head :: GHC.Stack.Types.HasCallStack => [a] -> a
  Devuelve la cabeza de una lista, su primer elemento.

  tail :: GHC.Stack.Types.HasCallStack => [a] -> [a]
  Devuelve la cola de una lista, sublista sin el primer elemento.

  init :: GHC.Stack.Types.HasCallStack => [a] -> [a]
  Devuelve todos los elementos de una lista excepto el último. La lista no debe estar vacía.

  take :: Int -> [a] -> [a]
  Devuelve una lista de longitud n, con lo n primeros valores de una lista dada.

  drop :: Int -> [a] -> [a]
  Devuelve xs sin los primeros n elementos.  

--}

-- Ejercicio 2 ::

-- valorAbsoluto.

valorAbsoluto :: Float -> Float
valorAbsoluto n | n < 0 = (n*(-1)) - (parteDecimal (n*(-1)))         
                | otherwise = n - (parteDecimal n)


parteDecimal :: Float -> Float
parteDecimal n | n < 1 = n 
               | otherwise = parteDecimal (n-1)
 


-- bisiesto.

bisiesto :: Int -> Bool
bisiesto n | n `mod` 4 == 0 = True 
           | n `mod` 400 == 0 && n `mod` 100 == 0 = True
           | otherwise = False


-- factorial.

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial n-1



-- cantidadDivisoresPrimos

cantidadDivisoresPrimos :: Int -> Int
cantidadDivisoresPrimos 1 = 1
cantidadDivisoresPrimos n | esPrimo n == True =  1 + cantidadDivisoresPrimos (n-1) 
                          | otherwise = cantidadDivisoresPrimos (n-1)


esPrimo :: Int -> Bool 
esPrimo n = (cantidadDivisores n) == 1


cantidadDivisores :: Int -> Int
cantidadDivisores n = cantidadDivisoresAux n (n-1)

cantidadDivisoresAux :: Int -> Int -> Int
cantidadDivisoresAux n 1 = 1
cantidadDivisoresAux n m | n `mod` m == 0 = 1 + cantidadDivisoresAux n (m-1)
                         | otherwise = cantidadDivisoresAux n (m-1)


-- Ejercicio 3

data Maybe a = Nothing | Just a
data Either a b = Left a | Right b



inverso :: Float -> Maybe Float 
inverso n | n /= 0 &&  n * (1/n) == 1 = Just n
          | otherwise = Nothing


aEntero :: Either Int Bool -> Int
aEntero (Left a)  =  a
aEntero (Right b) = if b == True then 1 else 0


-- Ejercicio 4 ::


limpiar :: [Char] -> [Char] -> [Char]
limpiar  [] (ys)    =  ys
limpiar (x:xs) (ys) =  ( limpiar xs (ys \\ [x]) )


difPromedio :: [Float] -> [Float]
difPromedio xs = map (\x -> x - promedio) xs
       where promedio = sum xs / fromIntegral (length xs)

totalesIguales :: [Int] -> Bool
totalesIguales [x] = True
totalesIguales (x:y:xs) = x == y && totalesIguales (y:xs)


-- Ejercicio 5 ::

data AB a  = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB (Bin i r d) = False


negacionAB :: AB Bool -> AB Bool 
negacionAB  Nil = Nil
negacionAB (Bin i r d) = Bin (negacionAB i) (not r) (negacionAB d)


productoAB :: AB Int -> Int 
productoAB (Nil) = 1
productoAB (Bin i r d) = r * (productoAB i) * (productoAB d)











