
-- data TIPO = Constructor | Constructor
-- data TIPO = Constructor param param | Constructor param


data AB a = Nil | Bin (AB a) a (AB a)


-- Como hago fold sobre tipos de datos algebraicos?

-- fold recibe tantos parametros como constructores tenga y uno extra para pasar el tipos



foldAB :: (b -> a -> b -> b) -> b -> AB a -> b
foldAB cBin cNil Nil = cNil
foldAB cBin cNil (Bin i r d) = cBin (foldAB cBin cNil i) r (foldAB cBin cNil d)

--foldAB funCons1 funCons2... tipo  
--foldAB cBin     cNil        (Bin i r d)

--podemos ver que para cada constructor se pasa como parametro una funcion
--y el parametro que recibe el tipo.
--hay tantos casos como constructores.

--cBin y cNil funciones.

-- (Bin i r d) desestructura el AB , es similar a decir (x:xs) para un AB


