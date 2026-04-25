data ABNV a = Hoja a 
              | Uni a (ABNV a) 
              | Bi (ABNV a) a (ABNV a) 

ejemplo:
abnv = Bi (Uni 2 (Hoja 1)) 3 (Bi (Hoja 4) 5 (Uni 2 (Hoja 7)))

foldABNV :: (a->b) -> (a->b->b) -> (b->a->b->b) -> ABNV a -> b
foldABNV fHoja fUni fBi t = case t of
   Hoja r -> fHoja r
   Uni  r d -> fUni x (rec d)
   Bi   i r d -> fBi (rec i) r (rec d)
   where rec = foldABNV fHoja fUni fBi 

recABNV :: (a->b) -> (a->ABNV a->b->b) -> (a->ABNV a->ABNV a->b->b->b)-> ABNV a -> b
recABNV fHoja fUni fBi t = case t of
   Hoja r -> fHoja r
   Uni  r d -> fUni x d (rec d)
   Bi   i r d -> fBi i r d (rec i) (rec d)
   where rec = recABNV fHoja fUni fBi 

reemplazarUno :: Eq a => a -> a -> ABNV a -> ABNV a
reemplazarUno = 


-- Recorrido en preorden (raíz primero)
preorden :: ABNV a -> [a]
preorden = recABNV 
  (\x -> [x])
  (\x rd rec -> x : rec)
  (\x ri rd reci recd -> r : reci ++ recd)


-- Recorrido en inorden (izq-raíz-der)
inorden :: ABNV a -> [a]
inorden = recABNV 
  (\x -> [x])
  (\x rd rec -> x : rec)
  (\x ri rd reci recd -> reci ++ [x] ++ recd)


elemABNV :: Eq a => a -> ABNV a -> Bool
elemABNV x = foldABNV 
  (==x) 
  (\k rec -> k == x || rec) 
  (\rec1 k recd -> k == x || reci || recd)

reemplazarUno :: Eq a => a -> a -> ABNV a -> ABNV a
reemplazarUno x  y = recABNV 
  (\r -> if r == x then Hoja y else Hoja r) 
  (\r d rec -> if r == x then Uni y d else Uni r rec)
  (\i r d reci recd -> if (r == x ) 
                       then  Bi i y d else 
                       (if elemABNV x i 
                           then Bi reci i r d
                           else Bi i k recd
                        )

nivel :: ABNV a -> Int -> [a]
nivel k =  foldABNV
  (\x -> \n -> if n == 0  then [x] else [])
  (\x rec -> \n -> if n == 0 then [x] else rec n-1 )
  (\x reci recd -> 
       \n -> if  n == 0 then [x] else (reci n-1) ++ (recd n-1)



