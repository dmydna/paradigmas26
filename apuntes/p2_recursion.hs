{--Apunte--}


{-------------------------------------------------------------------

Recursión estructural (foldr):
-- El caso base devuelve un valor fijo z.
-- El caso recursivo se escribe usando (cero, una, o muchas veces) x, y la llamada
   recursiva (en foldr es (foldr f z xs) , en map es map f xs ). No se utiliza ni xs, 
   ni otros llamados recursivos
-}


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x : xs) = f x (foldr f z xs)


--ejemplo de map usando foldr 
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x rec -> f x : rec) []

--ejemplo de armarPares donde rec osea el acumulador es una funcion con un argumento.
armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr (\x (rec ys) -> if null ys then [] else (x, head ys) : rec (tail ys)) (const [])


--observar que rec en la primer iteracion es const [] osea devuelve []
--y en las demas rec ys -> (if.. then.. else) 

{----------------------------------------------------------------------

Recursión primitiva (recr):
-- Casi igual a la estructural; tiene de diferencia que podés usar el xs.
-- Recursión primitiva ⟹ Recursión estructural (la 2da es un caso particular de la 1ra)

-}

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x : xs) = f x xs (recr f x xs)


recAB :: b -> (a -> AB a -> AB a -> b -> b -> b) -> AB a -> b
recAB z _ Nil = z
recAB z f (Bin izq r der) = f r izq der (recAB z f izq) (recAB z f der)


--ejemplo de sumarLista usando recr
sumarLista :: [Integer] -> Integer
sumarLista = recr (\x xs rec -> rec + x) 0

{-Nota:-}

-- "recr (\x xs rec -> rec + x) 0"
-- podemos pensar (\x xs rec -> ... ) como f x xs (recr f x xs) 
-- donde la funcion anonima "(\..-> ..)" es f  sus parametros son "x" "xs" y "rec" que es "(recr f x xs)" un "acumulador"
-- por otra parte 0 es el valor incial de rec osea su acumulador.

-- rec osea el acumulador pueden ser de cualquier tipo, un tipo T, una lista o una funcion que recibe parametos

{---------------------------------------------------------------------

Recursión iterativa (foldl):
-- El caso base devuelve el acumulador acc.
-- El caso recursivo invoca inmediatamente a (f acc x).

-}


foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x : xs) = foldl f (f acc x) xs
-- ejemplo reverse usando foldl
reverse :: [a] -> [a]
reverse = foldl (\acc x -> x : acc) []


{-----------------------------------------------------------------------}


{--RESUMEN RAPIDO--}

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr funcion valorInicial [] = valorInicial
recr funcion valorInicial (elementoActual : restoDeLaLista) =
    funcion elementoActual restoDeLaLista (recr funcion valorInicial restoDeLaLista)

sumarLista :: [Integer] -> Integer
sumarLista = recr (\elementoActual restoDeLaLista acumulador -> acumulador + elementoActual) 0


