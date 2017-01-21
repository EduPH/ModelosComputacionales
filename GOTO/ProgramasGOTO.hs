module ProgramasGOTO where
import GOTO
import MacrosNew

-- | Programa lineal que devuelve la constante 3 para cualquier variable
-- de entrada.

programaCte3 = Pr [Incremento y (E [] 0), 
                   Incremento y (E [] 0), 
                   Incremento y (E [] 0)]
-- | Ejemplos
-- >>> programaCte3
--      Y<-Y+1
--      Y<-Y+1
--      Y<-Y+1

-- | Programa que devuelve 1 si X=0, y devuelve 0 en caso contrario.

programaIndicadorDel0 = Pr [Condicional (E [] 0) x  (E "E" 0) , 
                            Incremento y (E [] 0)]
-- | Ejemplos
-- >>> programaIndicadorDel0
--      IF X/=0 GOTO [E] 
--      Y<-Y+1

-- | Y <- X

valorDeX :: Programa
valorDeX = Pr [ Condicional (E [] 0) x (E "A" 0),
                Incremento z (E [] 0),
                Condicional (E [] 0) z (E "E" 0),
                Decremento x (E "A" 0),
                Incremento y (E [] 0),
                Condicional (E [] 0) x (E "A" 0)]
-- | Ejemplos
-- >>> valorDeX
--      IF X/=0 GOTO [A] 
--      Z<-Z+1
--      IF Z/=0 GOTO [E] 
-- [A]  X<-X-1
--      Y<-Y+1
--      IF X/=0 GOTO [A] 

-- | Programa para el producto
producto = Pm [valV (VarWork [2]) (VarIn [2]) (E "" 0),
               CondM (E "B" 0) z (E "A" 0),
               IncM (VarWork [4]) (E "" 0),
               CondM (E "" 0) (VarWork [4]) (E "E" 0),
               DecM (VarWork [2]) (E "A" 0),
               IncM y (E "" 0),
               valV (VarWork [1]) (VarIn [1]) (E "" 0),
               valV (VarWork [3]) y (E "" 0),
               CondM (E "B" 2) (VarWork [3]) (E "A" 2),
               IncM (VarWork [5]) (E "" 0),
               CondM (E "" 0) (VarWork [5]) (E "E" 2),
               DecM (VarWork [3]) (E "A" 2),
               IncM (VarWork [1]) (E "" 0),
               IncM (VarWork [6]) (E "" 0),
               CondM (E "" 0) (VarWork [6]) (E "B" 2),
               valV y (VarWork [1]) (E "E" 2),
               IncM (VarWork [7]) (E "" 0),
               CondM (E "" 0) (VarWork [7]) (E "B" 0) ]
                


-- | Ejemplos
-- >>> let p = Pm [IncM x (E "" 0), IncM x (E "" 0) , IncM x (E "" 0), valV z x (E "" 0), valV y z (E "" 0)]
-- >>> let q = progM2prog p
-- >>> q
--      X<-X+1
--      X<-X+1
--      X<-X+1
-- [A3] Z<-Z-1
--      IF Z/=0 GOTO [A3]
-- [A2] IF X/=0 GOTO [B2]
--      Z26<-Z26+1
--      IF Z26/=0 GOTO [C2]
-- [B2] X<-X-1
--      Z<-Z+1
--      Z30<-Z30+1
--      Z27<-Z27+1
--      IF Z27/=0 GOTO [A2]
-- [C2] IF Z30/=0 GOTO [D2]
--      Z28<-Z28+1
--      IF Z28/=0 GOTO [Q2]
-- [D2] Z30<-Z30-1
--      X<-X+1
--      Z29<-Z29+1
--      IF Z29/=0 GOTO [C2]
-- [Q2] Z31<-Z31+1
-- [A5] Y<-Y-1
--      IF Y/=0 GOTO [A5]
-- [A4] IF Z/=0 GOTO [B4]
--      Z42<-Z42+1
--      IF Z42/=0 GOTO [C4]
-- [B4] Z<-Z-1
--      Y<-Y+1
--      Z46<-Z46+1
--      Z43<-Z43+1
--      IF Z43/=0 GOTO [A4]
-- [C4] IF Z46/=0 GOTO [D4]
--      Z44<-Z44+1
--      IF Z44/=0 GOTO [Q4]
-- [D4] Z46<-Z46-1
--      Z<-Z+1
--      Z45<-Z45+1
--      IF Z45/=0 GOTO [C4]
-- [Q4] Z47<-Z47+1
-- >>> let vars = variablesDe q
-- >>> let trab = [(a,0)| a <- vars, esZ a]
-- >>> let vs = [(x,0),(y,0)]++trab
-- >>> ejecutaP q vs
-- (Y,3)
