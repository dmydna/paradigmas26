data RoseTree a = Rose a [RoseTree a] deriving Show

rose = Rose 3 [Rose 2 [],
               Rose 1 [Rose 5 []],
               Rose 4 []]

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose r rs) = f r (map rec rs)
  where
    rec = foldRose f
    
ramas :: RoseTree a -> [[a]]
ramas = foldRose (\x rec -> if null rec 
                            then [[x]]
                            else map (x:) (concat rec))

-- Nota: aca vemos que rec es el resultado de la recursion es decir [[a]]

hojasRT :: Rosetree a -> [a]
hojasRT = foldRose (\r -> recHijos -> if null recHijos then [r] else concat recHijos)

alturaRT :: Rosetree a -> Int
alturaRT (Rose _ []) = 1
alturaRT (Rose _ hijos) = 1 + maximum (map alturaRT hijos)

