data Buffer a = Empty | Write Int a (Buffer a) | Read Int (Buffer a)

buf = Write 1 "a" $ Write 2 "b" $ Write 1 "c" $ Empty

foldBuffer ::  b -> (Int -> a -> b ->b) -> (Int -> b -> b) -> Buffer a -> b
foldBuffer fEmpty fWrite fRead t = 
  case t of 
     Empty -> fEmpty 
     Write n x m -> fWrite n x (rec m)
     Read  n m   -> fRead n (rec m)
     where rec = foldBuffer fEmpty fWrite fRead  

recBuffer :: b -> (Int -> a -> Buffer a ->b) -> (Int -> Buffer a -> b) -> Buffer a -> b
recBuffer fEmpty fWrite fRead t = 
  case t of 
     Empty -> fEmpty
     Write n x m -> fWrite n x m (rec m)
     Read  n m   -> fRead n m (rec m)
     where rec = recBuffer fEmpty fWrite fRead


posicionesOcupadas :: Buffer a -> [Int]
posicionesOcupadas = 
  foldBuffer [] 
            (\n _ rec -> union [n] rec)
            (\n _ rec -> filter rec(\e /= n))

contenido :: Int -> Buffer a -> Maybe a
contenido e = 
  foldBuffer Nothing 
            (\n x rex -> if n == e then Just x else rec)
            (\n x rex -> if n == e then Nothing x else rec)

puedeCompletarLecturas :: Buffer a -> Bool
puedeCompletarLecturas = 
  recBuffer True
            (\ _ _ _ rec -> rec)
            (\n b rec -> (elem n (posicionesOcupadas b)) &&rec )

deshacer :: Buffer a -> Int -> Buffer a
deshacer = 
  recBuffer (const Empty) 
            (\n x b rec -> \e -> 
               if e == 0 then (Write n x b))
            (\n b rec -> \e ->  
               if e == 0 then (Read n b) else rec(e-1))


