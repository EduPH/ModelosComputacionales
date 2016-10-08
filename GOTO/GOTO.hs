
module GOTO where

-- El tipo de dato variable.
type Nombre = String
type Indice = [Int]
type Valor = Int
data Variable = VarIn Indice Valor
              | VarOut Valor
              | VarWork Indice Valor


-- Función auxiliar
showInts :: Show a => [a] -> [Char]
showInts []      = ""
showInts [i]     = show i
showInts (i:is') = show i ++ "_" ++ showInts is'

-- Las variables de entradas siempre serán denotadas por la letra X, la
-- de salida por la letra Y y las de trabajo por la letra Z.
-- Representación:
instance Show Variable where
  show ( VarIn [] i)    = "X" 
  show ( VarIn [i] j)   = "X" ++ show i
  show ( VarIn is j)    = "X" ++ showInts is
  show ( VarOut i)      = "Y" 
  show ( VarWork [] i)  = "Z" 
  show ( VarWork [i] j) = "Z" ++ show i
  show ( VarWork is j)  = "Z" ++ showInts is

-- Valor de la variable
valor :: Variable -> Valor
valor (VarIn i v) = v
valor (VarOut v) = v
valor (VarWork i v) = v

-- Como los valores iniciales de la variable de salida y las de trabajo son siempre es 0, las
-- definimos previamente.

y,z :: Variable
y = VarOut 0
z = VarWork [] 0

-- Definimos las instrucciones

type Etiqueta = String
 
data Instruccion =  Incremento Variable Etiqueta
                  | Decremento Variable Etiqueta
                  | Condicional Etiqueta Variable Etiqueta






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

-- Ejemplo
-- λ> Incremento y "A"
-- [A] (Y=0)<-(Y=0)+1

data Programa = Pr [Instruccion]

instance Show Programa where
    show (Pr [i]) = show i
    show (Pr (i:is)) = (show i) ++ "\n" ++ (show (Pr is))

-- Ejemplo de programa que calcula la función identidad
-- λ> let x = VarIn [] 0
-- λ> let z = VarWork [] 0
-- λ> Pr [Condicional [] x "B", Incremento z [], Condicional [] z "E", Decremento x "B", Incremento y [], Condicional [] x "B"]
--  IF X/=0 GOTO [B]
--  Z<-Z+1
--  IF Z/=0 GOTO [E]
-- [B] X)<-X-1
--  Y<-Y+1
--  IF X/=0 GOTO [B]

programaIdentidad :: Programa
programaIdentidad = Pr [Condicional [] (VarIn [] 0) "B", 
                        Incremento z [], Condicional [] z "E", 
                        Decremento (VarIn [] 0) "B", 
                        Incremento y [], 
                        Condicional [] (VarIn [] 0) "B"]

-- Cambio del valor de una variable
cambiaVal :: Variable -> Valor -> Variable
cambiaVal (VarIn i v) v' =   VarIn i v'
cambiaVal (VarOut v) v' = VarOut v'
cambiaVal (VarWork i v) v' = VarWork i v'


-- Devuelve la etiqueta de una instrucción
etiqueta :: Instruccion -> Etiqueta
etiqueta (Decremento v l) = l
etiqueta (Incremento v l) = l
etiqueta (Condicional l v l') = l

-- Búsqueda de una instrucción con una etiqueta dada
buscaI :: Programa -> Etiqueta -> Instruccion
buscaI (Pr is) e = head  [i | i<- is, etiqueta i == e]

