
{-|
Module      : GOTO
Implementación del lenguaje GOTO
-}

module GOTO where
import Data.List

-- * Implementación del lenguaje GOTO
-- ==================================

-- | Se definen los distintos tipos de datos que conforman las
-- variables. Una variable puede ser de entrada, salida o de trabajo, y
-- se denotan por x,y,z, respectivamente. Además, puede llevar índices.

type Indice = [Int]
-- | Tipo de dato 'Variable' 
data Variable = VarIn Indice 
              | VarOut 
              | VarWork Indice 
                deriving Eq

-- | Un estado es un par de una variable y el valor asociado a ella.

type Valor = Int
-- | Tipo de dato 'Estado'
type Estado =  (Variable, Valor)

-- | Función auxiliar para la representación por pantalla.

showInts :: Show a => [a] -> [Char]
showInts []      = ""
showInts [i]     = show i
showInts (i:is') = show i ++ "_" ++ showInts is'

-- | Definimos la representación de las variables por pantalla:

instance Show Variable where
  show ( VarIn [] )   = "X" 
  show ( VarIn [i])   = "X" ++ show i
  show ( VarIn is)    = "X" ++ showInts is
  show ( VarOut  )    = "Y" 
  show ( VarWork [])  = "Z" 
  show ( VarWork [i]) = "Z" ++ show i
  show ( VarWork is)  = "Z" ++ showInts is

-- | Ejemplos
-- >>> VarIn [2]
-- X2
-- >>> VarOut 
-- Y
-- >>> VarWork [15]
-- Z15

-- | La función (valor e) determina el valor de la variable en un estado.

valor :: Estado -> Valor
valor (VarIn i,v) = v
valor (VarOut, v) = v
valor (VarWork i, v) = v

-- | Ejemplos
-- >>> valor (x,3)
-- 3

-- | La función (esZ v) caracteriza las variables de trabajo.

esZ :: Variable -> Bool
esZ (VarWork _) = True
esZ _ = False

-- | Ejemplos
-- >>> esZ (VarWork [15])
-- True
-- >>> esZ (VarIn [15])
-- False

esX :: Variable -> Bool
esX (VarIn _) = True
esX _ = False

-- | Definimos las variables (x,y,z) para facilitar su uso en la
-- definición de programas. 

x,y,z :: Variable
x = VarIn []
y = VarOut 
z = VarWork [] 

-- | La función (indice v) calcula el índice de una variable.

indice :: Variable -> Int
indice (VarWork []) = 0
indice (VarWork [i]) = i
indice (VarIn []) = 0
indice (VarIn [i]) = i

-- | Ejemplos
-- >>> indice x
-- 0
-- >>> VarWork [23]
-- Z23
-- >>> indice (VarWork [23])
-- 23

-- | Definimos el tipo de dato (Etiqueta).

data Etiqueta = E String Int
                deriving Eq
 
-- | Se definen las instrucciones incremento, decremento y condicional.

data Instruccion =  Incremento Variable Etiqueta
                  | Decremento Variable Etiqueta
                  | Condicional Etiqueta Variable Etiqueta
                  | SKIP Etiqueta
                    deriving Eq

-- | Representación de las etiquetas e instrucciones por pantalla:

instance Show Etiqueta where
    show (E "" n) = "    "
    show (E str 0) = "[" ++ str ++ "] "
    show (E str n) = "["++ str ++ show n ++ "]"

-- | Ejemplos
-- >>> E "A" 0
-- [A] 
-- >>> E "A" 3
-- [A3]     
-- >>> E "B" 0
-- [B] 

instance Show Instruccion where
     show (Incremento v l)     = show l ++ " " ++ show v ++ "<-" 
                                 ++ show v ++"+" ++ (show 1)
     show (Decremento v l)     = show l ++ " "  ++show v ++ "<-" 
                                 ++ show v ++"-" ++ (show 1)
     show (Condicional l v l') = show l ++ " " ++ "IF" ++ " "  
                                 ++ show v ++ "/=" ++ (show 0)
                                 ++" "++ "GOTO" ++" " ++ show l'
     show (SKIP l) = show l ++ " " ++ "Y <- Y"

-- | Un programa se define como una lista de instrucciones.

data Programa = Pr [Instruccion]

-- | Representación del tipo de dato Programa.

instance Show Programa where
    show (Pr [i]) = show i
    show (Pr (i:is)) = (show i) ++ "\n" ++ (show (Pr is))

-- | Definimos un programa ejemplo.

programaIdentidad :: Programa
programaIdentidad = Pr [Condicional (E [] 0) x (E "B" 0), 
                        Incremento z (E [] 0), 
                        Condicional (E [] 0) z (E "E" 0), 
                        Decremento x (E "B" 0), 
                        Incremento y (E [] 0), 
                        Condicional (E [] 0) x (E "B" 0)]

-- | Ejemplos
-- >>> programaIdentidad
--      IF X/=0 GOTO [B] 
--      Z<-Z+1
--      IF Z/=0 GOTO [E] 
-- [B]  X<-X-1
--      Y<-Y+1
--      IF X/=0 GOTO [B] 


-- * Ejecución de programas
-- ========================

-- | Cambio del valor de una variable en un estado.

cambiaVal :: Estado -> Valor -> Estado
cambiaVal (VarIn i, v) v' =   (VarIn i,v')
cambiaVal (VarOut, v) v' = (VarOut, v')
cambiaVal (VarWork i, v) v' = (VarWork i, v')

-- | Ejemplos
-- >>> cambiaVal (x,3) 4
-- (X,4)

-- | Etiqueta de una instrucción.

etiqueta :: Instruccion -> Etiqueta
etiqueta (Decremento v l) = l
etiqueta (Incremento v l) = l
etiqueta (Condicional l v l') = l
etiqueta (SKIP l) = l

-- | Ejemplos
-- >>> Incremento x (E "A" 0)
-- [A]  X<-X+1
-- >>> etiqueta (Incremento x (E "A" 0))
-- [A]

-- | Lista de etiquetas de un programa.

listaEtiquetas :: Programa -> [Etiqueta]
listaEtiquetas (Pr is) = map (etiqueta) is

-- | Ejemplos
-- >>> listaEtiquetas programaIdentidad
-- [    ,    ,    ,[B] ,    ,    ]

-- | Variable sobre la que actúa una instrucción.

varInstruccion :: Instruccion -> Variable
varInstruccion (Incremento v _) = v
varInstruccion (Decremento v _) = v
varInstruccion (Condicional _ v _) = v

-- | Ejemplos
-- >>> Incremento x (E "" 0)
--      X<-X+1
-- >>> varInstruccion (Incremento x (E "" 0))
-- X

-- | Búsqueda de la posición de una instrucción con una etiqueta dada.

buscaI :: Programa -> Etiqueta -> Int
buscaI (Pr []) e = 0
buscaI (Pr (i:is)) e | etiqueta i == e = 1
                     | otherwise = 1+ buscaI (Pr is) e

-- | Ejemplos
-- >>> buscaI programaIdentidad (E "B" 0)
-- 4

-- | Valor de una variable en una lista de estados.

valorP :: Variable -> [Estado] -> Valor
valorP v xs = head [valor x | x <- xs, (fst x) ==v]

-- | Ejemplos
-- >>> valorP x [(x,7),(y,0),(z,3)]
-- 7

-- | Función auxiliar para Incremento: 

suma1 :: Variable -> [(Variable, Valor)] -> [Estado]
suma1 v [] = []
suma1 v (x':xs) | fst x' == v = (cambiaVal x' (valor x' +1)):xs
                | otherwise = x': (suma1 v xs)

-- | Función auxiliar para Decremento:

resta1 :: Variable -> [(Variable, Valor)] -> [Estado]
resta1 v [] = []
resta1 v (x':xs) | fst x' == v && valor x' /= 0 = 
                     (cambiaVal x' (valor x'-1)):xs
                 | fst x' == v && valor x' == 0 = (x':xs)
                 | otherwise = x': (resta1 v xs)

-- | Ejemplos
-- >>> suma1 x [(x,7),(y,0),(z,3)]
-- [(X,8),(Y,0),(Z,3)]
-- >>> resta1 x [(x,7),(y,0),(z,3)]
-- [(X,6),(Y,0),(Z,3)]
 
-- | Función para obtener la variable de salida y su valor.

salida :: [Estado] -> Estado
salida xs = head [v | v <- xs, fst v == VarOut]

-- | Ejemplos
-- >>> salida [(x,7),(y,0),(z,3)]
-- (Y,0)

-- | Caracterización de las etiquetas de salida.

etSalida :: Programa -> Etiqueta -> Bool
etSalida (Pr []) e = True
etSalida (Pr is) e = null [i | i<- is, etiqueta i == e]

-- | Ejemplos
-- >>> etSalida programaIdentidad (E "B" 0)
-- False
-- >>> etSalida programaIdentidad (E "E" 0)
-- True

-- | La función (ejecuta n p xs) ejecuta el paso n del programa p según
-- una lista de estados xs. 

ejecuta :: Int -> Programa -> [Estado] -> Estado
ejecuta n p@(Pr is) xs = aux (is !! (n-1))
    where 
      aux (Incremento v e) | n < length is =  ejecuta (n+1) p (suma1
                                                                  v xs)
                           | otherwise = salida (suma1 v xs)
      aux (Decremento v e) | n < length is =  ejecuta (n+1) p (resta1
                                                                  v xs)
                           | otherwise = salida (resta1 v xs)
      
      aux (Condicional e v e') | valorP v xs /= 0 && not ( etSalida p e') =  
                                   ejecuta (buscaI p e') p xs
                               | valorP v xs /= 0 && etSalida p e' = 
                                   salida xs
                               | otherwise = 
                                   if (n < length is) then 
                                       ejecuta (n+1) p xs 
                                   else (salida xs)
                                                 
      aux (SKIP e) = ejecuta (n+1) p xs

-- | La función (ejecutaP p) ejecuta el programa p según los estados xs.
     
ejecutaP :: Programa -> [Estado] -> Estado
ejecutaP p xs = ejecuta 1 p xs

-- | Ejemplo
-- >>> ejecutaP programaIdentidad [(x,8),(y,0),(z,0)]
-- (Y,8)

-- | Lista de las variables de un programa sin repetir.

variablesDe :: Programa -> [Variable]
variablesDe (Pr ps) = nub (map (varInstruccion) ps)

-- | Lista de las variables de un programa, repitiéndolas. 

listaVariablesDe :: Programa -> [Variable]
listaVariablesDe (Pr ps) = map (varInstruccion) ps

-- | Ejemplos
-- >>> variablesDe programaIdentidad
-- [X,Z,Y]
-- >>> listaVariablesDe programaIdentidad
-- [X,Z,Z,X,Y,X]
