
module GOTO where

-- El tipo de dato variable.
type Nombre = String
type Indice = [Int]
type Valor = Int
data Variable = VarIn Indice 
              | VarOut 
              | VarWork Indice 
                deriving Eq
type Estado =  (Variable, Valor)

-- Función auxiliar
showInts :: Show a => [a] -> [Char]
showInts []      = ""
showInts [i]     = show i
showInts (i:is') = show i ++ "_" ++ showInts is'

-- Las variables de entradas siempre serán denotadas por la letra X, la
-- de salida por la letra Y y las de trabajo por la letra Z.
-- Representación:
instance Show Variable where
  show ( VarIn [] )    = "X" 
  show ( VarIn [i])   = "X" ++ show i
  show ( VarIn is)    = "X" ++ showInts is
  show ( VarOut  )      = "Y" 
  show ( VarWork [])  = "Z" 
  show ( VarWork [i]) = "Z" ++ show i
  show ( VarWork is)  = "Z" ++ showInts is

-- Valor de la variable en un estado
valor :: Estado -> Valor
valor (VarIn i,v) = v
valor (VarOut, v) = v
valor (VarWork i, v) = v

-- Definimos variables para facilitar su uso
y,z :: Variable
y = VarOut 
z = VarWork [] 
x = VarIn []

-- Definimos las instrucciones
type Etiqueta = String
 
data Instruccion =  Incremento Variable Etiqueta
                  | Decremento Variable Etiqueta
                  | Condicional Etiqueta Variable Etiqueta
                    deriving Eq






-- Representación de las instrucciones:

instance Show Instruccion where
     show (Incremento v []) = " " ++ show v ++ "<-" ++show v ++"+" ++
                             (show 1)
     show (Incremento v l)  = "["++l++"]" ++ " " ++ show v ++ "<-" ++show v ++"+" ++
                             (show 1)
     show (Decremento v []) = " "  ++show v++")" ++ "<-" ++ show v ++"-" ++
                             (show 1)
     show (Decremento v l) = "["++l++"]" ++ " "  ++show v ++ "<-" ++ show v ++"-" ++
                             (show 1)
     show (Condicional [] v l') =  " " ++ "IF" ++ " "  ++ show
                                 v ++ "/=" ++ (show      0)
                              ++" "++ "GOTO" ++" " ++ "[" ++ l'++"]"
     show (Condicional l v l') = "["++l++"]" ++ " " ++ "IF" ++ " "  ++ show
                                 v ++ "/=" ++ (show
                                                                     0)
                              ++" "++ "GOTO" ++" " ++ "[" ++ l'++"]"

-- Tipo de dato para una lista de instrucciones
data Programa = Pr [Instruccion]

-- Representación del tipo de dato Programa
instance Show Programa where
    show (Pr [i]) = show i
    show (Pr (i:is)) = (show i) ++ "\n" ++ (show (Pr is))

-- Cambio del valor de una variable
cambiaVal :: Estado -> Valor -> Estado
cambiaVal (VarIn i, v) v' =   (VarIn i,v')
cambiaVal (VarOut, v) v' = (VarOut, v')
cambiaVal (VarWork i, v) v' = (VarWork i, v')

-- Devuelve la etiqueta de una instrucción
etiqueta :: Instruccion -> Etiqueta
etiqueta (Decremento v l) = l
etiqueta (Incremento v l) = l
etiqueta (Condicional l v l') = l

-- Obtener la variable de una instrucción
varInstruccion :: Instruccion -> Variable
varInstruccion (Incremento v _) = v
varInstruccion (Decremento v _) = v
varInstruccion (Condicional _ v _) = v

-- Búsqueda de la posición de una instrucción con una etiqueta dada
buscaI :: Programa -> Etiqueta -> Int
buscaI (Pr []) e = 0
buscaI (Pr (i:is)) e | etiqueta i == e = 1
                     | otherwise = 1+ buscaI (Pr is) e

-- Valor de una variable en la lista de estados
valorP :: Variable -> [Estado] -> Valor
valorP v xs = head [valor x | x <- xs, (fst x) ==v]


-- Funciones auxiliares para Incremento y Decremento: 
suma1 :: Variable -> [(Variable, Valor)] -> [Estado]
suma1 v [] = []
suma1 v (x':xs) | fst x' == v = (cambiaVal x' (valor x' +1)):xs
                | otherwise = x': (suma1 v xs)
resta1 :: Variable -> [(Variable, Valor)] -> [Estado]
resta1 v [] = []
resta1 v (x':xs) | fst x' == v = (cambiaVal x' (valor x' -1)):xs
                 | otherwise = x': (resta1 v xs)


-- Función para obtener la variable de salida
salida :: [Estado] -> Estado
salida xs = head [v | v <- xs, fst v == VarOut]

-- Ejecución de programas:
ejecuta n p@(Pr is) xs = aux (is !! (n-1))
    where 
      aux (Incremento v e) | n < length is =  ejecuta (n+1) p (suma1
                                                                  v xs)
                           | otherwise = salida (suma1 v xs)
      aux (Decremento v e) | n < length is =  ejecuta (n+1) p (resta1
                                                                  v xs)
                           | otherwise = salida (resta1 v xs)
      
      aux (Condicional e v e') | valorP v xs /= 0 && e'/= "E" =  ejecuta (buscaI p e') p xs
                               | valorP v xs /= 0 && e'== "E" = salida xs
                               | otherwise = if (n < length is) then
                                                 ejecuta (n+1) p xs else
                                                 (salida xs)
                                                 
     
ejecutaP :: Programa -> [Estado] -> Estado
ejecutaP p xs = ejecuta 1 p xs
