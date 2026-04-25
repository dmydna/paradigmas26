-- EJERCICIO PARCIAL
-- 2c2025

data FS = Arch String | Dir String [FS] deriving (Eq, Show)


foldFS :: (String -> b) -> (String -> [b] -> b) -> FS -> b
foldFS fArch fDir t = case t of
   Arch s -> fArch s
   Dir s xs -> fDir s (map rec xs)
      where rec = foldFS fArch fDir


recFS :: (String -> b) -> (String -> [FS] -> [b] -> b) -> FS -> b
recFS fArch fDir = case t of
   Arch s -> fArch s
   Dir s xs -> fDir s xs (map rec xs)
      where rec = recFS fArch fDir




rutas :: FS -> [String]
rutas = foldFS (\s -> [s])
               (\s subs -> s : (map (\ruta -> s ++ '/' ++ ruta)(concat subs) ))


-- Nota:
-- Vemos que la funcion rutas devuelve [string], esto seria b en el foldFS.
-- por lo que la firma de las funciones parametros de ruta quedarian asi.

-- (String -> [string]) ->                   // fArch
-- (String -> [[string]] -> string) ->       // fDir
-- FS -> string

-- Entonces subs es [[string]],lo podemos pensar que como el foldFS ya proceso toda 
-- la estructura con la funcion del caso base y
-- recibimos una lista de listas.

--- concat :: [[a]] -> [a]


valido :: FS -> Bool
valido = recFS (const true)
               (\s subFSS subsols -> (all id subsols)  &&
                length (map nombre subFSS) == length (nub (map nombre subFSS)))

nombreFS :: FS -> String 
nombreFS = case fs of
   (Arch s ) -> s
   (Dir s _) -> s


rutasPosibles xss = concatMap nombresConBarras [0..]
    where nombresConNBarras = foldNat (\rs -> [s] ++ '/' : s2 | s1 <-rs, s2 <- xss ) xss





