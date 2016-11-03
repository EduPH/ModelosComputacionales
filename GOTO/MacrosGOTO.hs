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
anula1 :: Variable -> Etiqueta -> [Estado] -> [Estado]
anula1 v e xs = ejecutaM (Pr [Decremento v e,
                             Condicional [] v e]) xs
-- Salto incondicional
goto e = Pr [Incremento z [], 
             Condicional [] z e]

-- Anular una variable
anula v = Pr [Decremento v "A", 
              Condicional [] v "A"]

-- v<-v'
copiaA v' v = undefined


type ProgramaM = [Programa]

-- Expansión de macros sin normalizar
expandeMacro :: ProgramaM -> Programa
expandeMacro ((Pr is):ps) = Pr (is++ aux ps)
    where
      aux [] = []
      aux ((Pr ts):ps) = ts++ aux ps

-- Normalización de una lista de listas de variables ordenadas
normalizaIndice :: [[Variable]] -> [[Variable]]
normalizaIndice [vs] = [vs]
normalizaIndice (vs:(ts:vss)) = [vs] ++ normalizaIndice ((aux ts (maxIndice vs)):vss) 
    where
      maxIndice vs =  maximum [ indice v | v <- vs, esZ v] 
      aux vs n = [incIndice n v | v <- vs] 
      incIndice n v | esZ v = VarWork [(indice v)+n+1]
                    | otherwise = v

-- Dada una lista de variables ordenadas, asociarlas a un programa
actualizaVar :: Programa -> [Variable] -> Programa
actualizaVar (Pr is) vs = Pr (map (asocia) (zip is vs))
    where
      asocia (Incremento _ e,v) = Incremento v e
      asocia (Decremento _ e,v) = Decremento v e
      asocia (Condicional e1 _ e2,v) = Condicional e1 v e2

-- Normalización de etiquetas

normalizaEtiqueta :: [Programa] -> [Programa]
normalizaEtiqueta = undefined -- Pendiente

-- Normalización de una lista de programas (Falta añadir normalización
-- de etiquetas)
normListPr :: [Programa] -> [Programa]
normListPr ps = [actualizaVar u v | (u,v)<- p]
    where 
      p = zip ps (normalizaIndice (map (listaVariablesDe) ps))
