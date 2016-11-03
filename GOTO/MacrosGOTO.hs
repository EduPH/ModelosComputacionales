module MacrosGOTO where
import GOTO
import ProgramasGOTO

-- Función que ejecuta una Macro devolviendo los cambios que realiza
-- sobre una lista de variables.

ejAuxM n p@(Pr is) xs = aux (is !! (n-1))
    where 
      aux (Incremento v e) | n < length is =  ejAuxM (n+1) p (suma1
                                                                  v xs)
                           | otherwise =  (suma1 v xs)
      aux (Decremento v e) | n < length is =  ejAuxM (n+1) p (resta1
                                                                  v xs)
                           | otherwise =  (resta1 v xs)
      
      aux (Condicional e v e') | valorP v xs /= 0 && e'/= "E" =  ejAuxM (buscaI p e') p xs
                               | valorP v xs /= 0 && e'== "E" =  xs
                               | otherwise = if (n < length is) then
                                                 ejAuxM (n+1) p xs else
                                                  xs

ejecutaM = ejAuxM 1                                                  
-- Hacer una variable 0
anula :: Variable -> Etiqueta -> [Estado] -> [Estado]
anula v e xs = ejecutaM (Pr [Decremento v e,
                             Condicional [] v e]) xs

