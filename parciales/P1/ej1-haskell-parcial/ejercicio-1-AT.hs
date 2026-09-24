-- Ej1. 1C2024 - Parcial I

data AT a = NilT | Tri a (AT a) (AT a) (AT a)

foldAT :: b -> (a -> b -> b -> b -> b) -> AT a -> b
foldAT cNil cTril t = case t of
            NilT -> cNil
            Tri x i m d -> cTri x (rec i) (rec m) (rec d)
               where rec = foldAT cNil Ctri

preorder :: AT a -> [a]
preorder = foldAT [] (\x ri rm rd -> x:(ri ++ rm ++ rd))

mapAT :: (a->b) -> AT a -> AT b 
mapAT f = foldAT (NilT) (\x ri rm rd -> Tri (fx) ri rm cd)

nivel:: AT a -> Int -> [a]
nivel = foldAT (\n -> []) (\x reci recm recd ->  \n -> 
                              if n == 0 
                              then [x] 
                              else (reci n-1) 
                                ++ (recm n-1) 
                                ++ (recd n-1)
                           )
















