module ProgramasGOTO where
import GOTO

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

