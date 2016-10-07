
module GOTO where

-- El lenguaje GOTO es un modelo secuencial, determinista con conjunto
-- de datos en los naturales.


-- El tipo de dato variable
type Nombre = String
type Indice = [Int]
type Valor = Int
data Variable = VarIn Indice Valor
              | VarOut Valor
              | VarWork Indice Valor


-- Función auxiliar
showInts []      = ""
showInts [i]     = show i
showInts (i:is') = show i ++ "_" ++ showInts is'

-- Representación
instance Show Variable where
  show ( VarIn [] i)    = "X" 
  show ( VarIn [i] j)   = "X" ++ show i
  show ( VarIn is j)    = "X" ++ showInts is
  show ( VarOut i)      = "Y" 
  show ( VarWork [] i)  = "Z" 
  show ( VarWork [i] j) = "Z" ++ show i
  show ( VarWork is j)  = "Z" ++ showInts is

valor :: Variable -> Valor
valor (VarIn i v) = v
valor (VarOut v) = v
valor (VarWork i v) = v
-- Como el valor inicial de la variable de salida siempre es 0, la
-- definimos previamente.
y = VarOut 0


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
     show (Decremento v l) = "["++l++"]" ++ " "  ++show v++")" ++ "<-" ++ show v ++"-" ++
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
