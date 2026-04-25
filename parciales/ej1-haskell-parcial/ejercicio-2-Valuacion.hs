data Prop = Var String | No Prop | Y Prop Prop | O Prop Prop | Imp Prop Prop

type Valuacion = String -> Bool

foldProp :: (String -> b) (b -> b) -> (b -> b ->b)  -> (b -> b ->b) -> (b -> b ->b) -> Prop b -> b
foldProp fVar fNot fY fO fImp t = case t of
   (Var s)     = fVar s
   (No p)      = fNot (recn p)
   (Y p1 p2)   = fY   (recn p1) (recn p2)
   (O p1 p2)   = fO   (recn p1) (recn p2)
              (Imp p1 p2) = fImp (recn p1) (recn p2)
   where 
     recn = foldProp fVar fNot fY fO fImp

-- nota: el tipo no recursivo "a" en este caso es String, "b" es el tipo recursivo.

recProp :: String -> b -> (Prop -> b -> b) -> (Prop -> Prop -> b -> b -> b) -> (Prop -> Prop -> b -> b -> b) -> (Prop -> Prop -> b -> b -> b) -> Prop -> b
recProp fVar fNot fY fO fImp = case t of:
   (String s)  = fVar s
   (No p)      = fNot p (recn p)
   (Y p1 p2)   = fY p1 p2 (recn p1) (recn p2)
   (O p1 p2)   = fO p1 p2 (recn p1) (recn p2)
   (Imp p1 p2) = fImp p1 p2 (recn p1) (recn p2)
   where 
     recn = recProp fVar fNot fY fO fImp



variableR :: Prop -> [String]
variableR = foldProp (\s-> [s]) id f f f
   where f = (\p1 p2 -> p1 ++ p2)

variables :: Prop -> [String]
variables p = eliminarRepetidos (variableR p)


eliminarRepetidos :: [a] -> [a]
eliminarRepetidos = foldr id (\x xs r -> if elem  x xs) then r else (x: r)










