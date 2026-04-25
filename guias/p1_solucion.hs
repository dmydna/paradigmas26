import Prelude hiding (subtract,sum,elem)

-- Ejercicio 1

max2 :: Ord a => (a,a) -> a
max2 (x,y) | x >= y = x
            | otherwise = y


max2Curry :: Ord a => a -> a -> (a -> a -> b) 
max2Curry = \x -> \y -> max2 (x,y)

normaVectorial :: Floating a => (a, a) -> a
normaVectorial (x, y) = sqrt (x^2 + y^2)

normaVectorialCurry :: Floating a => a -> a -> (a -> a -> b)
normaVectorialCurry = \x  \y -> normaVectorial (x, y) 


-- funcion currificada
--subtract :: Num a => a -> a -> a 
subtract = flip (-)


-- funcion currificada
--predecedor :: Num a => a -> a
predecedor n = subtract 1 n

-- funcion currificada
--evaluarEnCero :: Num a => (a->b) -> b
evaluarEnCero = \f -> f 0

--dosVeces :: Num a => a -> a
dosVeces = \f -> f * f


-- recibe una funcion y su argumentos intercambiados, devuelve c
flip :: (a -> b -> c) -> b -> a -> c


-- recibe una funcion y una lista, devuelve c
map :: (a -> b) -> [a] -> [b]

-- flipAll :: [(a->b->c)] -> [(b -> a -> c)]
-- recibe una lista de funciones con arg a y b y los swapea 
flipAll = map flip


--flipRaro :: (b -> a -> c) -> (a -> b -> c)
--flipRaro f a b = flip (flip f) a b
--recibe una funcion con flip aplicado y la desflipea?
flipRaro = flip flip



-- Ejercicio 2

curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \x -> \y -> f (x,y)


uncurry :: (a->b->c) -> (a,b)-> c
uncurry f = \(x, y) -> f x y


-- esta mal
curryN :: [((a,b) -> c)] -> [(a -> b -> c)]
curry f = \[x] -> \[y] -> f (x, y)




-- Ejercicio 3

{-

foldr generaliza la recursion para cualquier funcion

foldr :: func -> t -> xs -> t
  - func es una funcion 
  - t es el caso base de tipo T, es el valor incial del acumulador, 
    me indica el tipo de salida 
  - xs es una lista 


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x : xs) = f x (foldr f acc xs)

-}




-- acc = rec
-- foldr :: (a -> rec -> rec) -> rec -> [a] -> rec
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x : xs) = f x (foldr f acc xs)



sum :: (Num a) => [a] -> a
--sum xs = foldr (+) 0 xs
sum = foldr (+) 0

-- revisar



elem :: (Eq a) => a -> [a] -> Bool
elem e = foldr (\y rec -> (e == y) || rec) False
-- rec es una lista acumulador

-- elem 3 [1, 2, 3, 4]
-- (3 == 1) || ( (3 == 2) || ( (3 == 3) || ( (3 == 4) || False ) ) )



(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (:) ys xs 


filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\y rec -> if f y then y : rec else rec) []
-- rec es una lista acumulador


map :: (a -> b) -> [a] -> [b]
map = foldr (\y rec -> f y : rec) []
-- rec es una lista acumulador



mejorSegún :: (a -> a -> Bool) -> [a] -> a
mejorSegun _ [x] = x
mejorSegun criterio = foldr1 (\y maximo -> if criterio y maximo then y else maximo )


-- solucion con recursion

sumasParciales :: Numm a => [a] -> [a]
sumasParciales (x:xs) = sumasParcialesAux (x:xs) 0

sumasParcialesAux :: Num a => [a] -> a -> [a]
sumasParcialesAux [] _ = []
sumasParcialesAux (x:xs) acc = (x + acc) : sumasParciales xs (x + acc)

-- solucion del ejercicio

sumasParciales :: Num a => [a] -> [a]
sumasParciales [] = []
sumasParciales xs =  foldr (\y (r:rs) -> (y + r) : (r:rs) ) [0]

-- el valor inicial de (r:rs) que es una lista acumuladora es [0]



-- foldr itera la lista desde el ultimo elemento al primero.
sumAlt :: Num a => [a] -> a
sumAlt [] = 0
sumaAlt xs = snd ( foldr (\y (pos, acc) -> (pos+1, res) ) (0, 0) (reverse xs) )
          where res = if even pos then y + acc else y - acc


sumAltInv :: Num a => [a] ->  a
sumAltInv [] = 0
sumAltInv xs = snd ( foldr (\y (pos, acc) -> (pos+1, res) ) (0, 0) xs )
          where res = if even pos then y + acc else y - acc





-- Ejercicio 4


-- Ejercicio 5

-- NO es una funcion con recursion estructurada por se usan funciones como tail o null.

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs then [x] else x : elementosEnPosicionesPares (tail xs)


-- NO es una funcion con recursion estructurada pues usa null head tail en lugar de (x:xs).

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys then x : entrelazar xs [] else x : head ys : entrelazar xs (tail ys)


-- Ejercicio 6


recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z = z
recr f z (x:xs) = f x xs (recr f z xs)


--solucion normal

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna a (x:xs) = sacarUna (x:xs) a 1

sacarUna :: Eq a => [a] -> a -> a -> [a]
sacarUna [] _ _ = []
sacarUna x:xs a | x == a && ap == 1 = sacarUna a 0 xs
                | otherwise = x : sacarUna a 0 xs


sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e xs = snd $ recr (\y (acc, eliminado) ->  if e==y && not eliminado 
                                                    then (True, acc) 
                                                    else ( eliminado,y:acc) ) (False,[])  xs)
-- si uso foldr tendria que usar reverse pues foldr recorre la lista de derecha a izquierda.


insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado elem  xs = snd $ recr (\y (insertado,acc) ->  if elem <= y && not insertado 
                                                    then (True, e:y:acc) 
                                                    else (insertado,y:acc) ) (False,[])  xs)
             

-- Ejercicio 7

map :: (a->b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs


mapPares :: (a->a->b) -> [(a,a)] -> [(b,b)]
mapPares _ [] = []
mapPares f  =  foldr(\(x, y) acc -> (f x y, f y x) : acc) [] 
-- mapPares f ((x,y):xs) =  (f x y, f y x) : mapPares f xs




armarPares :: [a] -> [a] -> [(a,a)] 
armarPares xs ys = foldr (\x (rec ys) ->  if null ys then [] else (x, head ys ) : rec(tail ys)) (const [])




-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith _ [] _ = []
-- zipWith _ _ [] = []
-- zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

mapDoble :: (a->a->[a])->[a]->[a]->[b]
mapDoble _ [] _ = []
mapDoble _ _ [] = []
mapDoble f xs ys =  fst $ foldr (\x (i,otraLista,acc) ->  (res, pos, lista) ) ([],0, ys)
                  where (res, pos,lista) = (f x  head (drop i otraLista) : acc,i+1, otraLista)



-- Ejercicio 9

data Nat = Zero
         | Succ Nat

foldNat :: (Integer->b->b)-> b -> Integer -> b
foldNat fSucc fZero 0 = fZero
foldNat fSucc fZero n = fSucc n (foldNat fSucc fZero (n-1))


potenciaNat :: Integer -> Integer -> Integer
potenciaNat base = foldNat (\_ acc -> base * acc ) 1

-- Ejercicio 10


genLista :: a -> (a->a) -> Integer -> [a]
genLista size incr elem = foldl (\acc _ ->  if null acc then [elem] else acc ++ [incr (last acc)] ) [] [1...size]
--genLista size incr elem = [elem ..  elem + size-1 ]


--Ejercicio 11


data Polinomio a = X
                 | Cte a
                 | Suma (Polinomio a) (Polinomio a)
                 | Prod (Polinomio a) (Polinomio a)



foldPoli :: b->(a->b)->(a->b->b)-> (b->b->b) -> Polinomio a -> b
foldPoli z fCte fSuma fProd X = z
foldPoli z fCte fSuma fProd (Cta p) = fCte (Cta a)
foldPoli z fCte fSuma fProd (Suma p q) = fSuma (acc a) (acc b)
foldPoli z fCte fSuma fProd (Prod p q) = fProd 
 where
    acc = foldPoli casoX casoCte casoSuma casoProd


evaluar ::Num a => a -> Polinomio a -> a
evaluar n polinomio = foldPoli n id (+) (*)


-- Ejercicio 12

data AB a = Nil | Bin (AB a) a (AB a)


--foldr :: (a -> b -> b) -> b -> [a] -> b
--foldr f z [] = z
--foldr f z (x : xs) = f x (foldr f z xs)


foldAB :: a -> (b->a->b->b)-> AB a -> b
foldAB fNill fBin Nill = fNill
foldAB fNill fBin (Bin i r d) =  fBin (acc i) r (acc d) 
    where acc = foldAB fNill fBin 


--recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
--recr f z [] = z
--recr f z (x : xs) = f x xs (recr f x xs)

recAB :: b -> (a -> AB a -> AB a -> b -> b -> b) -> AB a -> b
recAB fNil _ Nil = fNil
recAB fNil fBin (Bin izq r der) = fBin r izq der (acc izq) (acc der)
        where acc = recAB z fin


esNil :: AB a -> bool
esNil Nil = True
esNil _ = False


altura :: AB a -> b
altura (Bin i r d)= foldAB 0 (\recI r recD -> 1 + max recI recD) 


cantNodos :: AB a -> b
cantNodos (Bin i r d) = foldAB 0 (\recI r recD -> 1 + recD + recI)


--mejorSegún :: (a -> a -> Bool) -> AB a -> a (VER EJERCICIO 3)
--esABB :: (a -> a -> Bool) -> AB a -> a


-- Ejercicio 13

-- ramas :: AB a -> b

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura = foldAB esNil (\recIzq r recDer arbol -> not (esNil arbol) && recIzq (hijoIzquierdo arbol) && recDer (hijoDerecho arbol) )


-- Ejercicio 14 

data AIH a = Hoja a | Bin (AIH a) (AIH a)

foldAIH :: (a->b) -> (a -> a -> b) -> AIH a -> b
foldAIH fHoja fBin (Hoja a) = fHoja
foldAIH fHoja fBin (Bin i d) = Bin (acc i) (acc d)
                         where acc = foldAIH fHoja fBin  


altura :: AIH a -> Integer
altura Bin i r d = foldAIH (const 1) (/recI r recD -> 1 + max recI recD) 

tamaño :: AIH a -> Integer
tamaño Bin i r d = foldAIH (const 0) (/recI r recD -> 1 + recI recD) 




-- Ejercicio 15

data Rosetree a = Rose a [Rosetree a]


--foldr :: (a -> b -> b) -> b -> [a] -> b
--foldr f z [] = z
--foldr f z (x : xs) = f x (foldr f z xs)


foldRT :: (a->[b]->b) -> Rosetree a -> b 
foldRT fRose (Rose r hijos) = fRose r (map (foldRT fRose) hijos)


-- RT a Lista 
hojasRT :: Rosetree a -> [a]
hojasRT = foldRT (\r recHijos -> if null recHijos then [r] else concat recHijos)

--distancias

alturaRT :: Rosetree a -> Int
alturaRT = foldRT (\r recHijos -> if null recHijos then 0 else 1 + maximum recHijos) 


-- EJERCICIO PARCIALES



-- donde b puede ser de cualquier tipo, 
-- y es el resultado de la recursion

--foldr :: (a -> recc -> recc) -> recc -> [a] -> recc
--foldr :: (a -> b -> b) -> b -> [a] -> b
--foldr f z [] = z
--foldr f z ts = case ts of: 
--             (x:xs) -> f x (rec xs)
--             where rec = foldr f z


-- uso
-- eliminarN :: [b] -> [b]
--eliminarN n xs= foldr (/x rec -> if x == n then rec else x:rec) [] xs


--longitud :: [b] -> Nat 
--longitud = foldr (/x n -> n + 1) [] 


--recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
--recr f z [] = z
--recr f z ts = case ts of: 
--          (x:xs) -> f x xs (rec x xs)
--          where rec = recr f z



data Prop = 
	Var String 
	| No Prop 
	| Y Prop Prop 
	| O Prop Prop 
	| Imp Prop Prop

type Valuacion = String -> Bool


foldProp :: (String->b) -> (b->b) -> (b->b->b) -> (b->b->b) -> (b->b->b) -> Prop -> b 
foldProp cVar _ _ _ _ (Var s) = (cVar s)
foldProp cVar cNo cY cO cImp t = case t of:
      No p      -> cNo (rec p) 
      (Y p1 p2)   -> cY (rec p1) (rec p2) 
      (O p1 p2)   -> cO (rec p1) (rec p2)
      (Imp p1 p2) -> cImp (rec p1) (rec p2)
        where rec = foldProp cVar cNo cY cO cImp




recProp :: (String->b) -> (Prop-> b -> b) -> (Prop->Prop->b->b->b) -> (Prop->Prop->b->b->b)  -> (Prop->Prop->b->b->b) -> Prop -> b
recProp cVar _ _ _ _ (Var s) = cVar s
recProp cVar cNo cY cO cImp t = caso p of:
      (No p)      -> cNo p (rec p) 
      (Y p1 p2)   -> cY p1 p2 (rec p1) (rec p2) 
      (O p1 p2)   -> cO p1 p2 (rec p1) (rec p2)
      (Imp p1 p2) -> cImp p1 p2 (rec p1) (rec p2)
        where rec = foldProp cVar cNo cY cO cImp


variables :: Prop -> [String] 
variables p = foldProp (\s -> [s]) id f f f
    where f = (\p1 p2 -> union p1 p2)


-- Parcial


data AT a = NilT | Tri a (AT a) (AT a) (AT a)


foldAT :: a -> (a -> b -> b -> b) -> AT a -> b
foldAT cNil cTri t = case t of:
	Nil -> cNil
	Tri x  i r d = cTri x (rec i) (rec r) (rec d)


preorder :: AT a -> [a]
preorder = foldAT fNil fTri
	where fNil = []
         fTri = (/x rec1 rec2 rec3 -> x : (rec1 ++ rec2 ++ rec3) )

mapAT :: (a->b) -> AT a -> AT b 
mapAT = foldAT fNil fTri
	where fNil = _
         fTri = (/x rec1 rec2 rec3 -> Tri f(x) (rec1) (rec2) (rec3) )



nivel :: AT a -> Int -> [a]
nivel = foldAT []
   (/x rec1 rec2 rec3 ->  if n == 0 then [x]
                                    else (rec1 n-1) ++ (rec2 n-1) ++ (rec3 n-1)








