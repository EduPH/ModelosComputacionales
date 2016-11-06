module ProgramasGOTO where
import GOTO

-- Programa lineal que devuelve la constante 3 para cualquier variable de entrada
programaCte3 = Pr [Incremento y (E [] 0), 
                   Incremento y (E [] 0), 
                   Incremento y (E [] 0)]

-- λ> programaCte3
--      Y<-Y+1
--      Y<-Y+1
--      Y<-Y+1

-- Programa que devuelve 1 si X=0, y devuelve 0 en caso contrario
programaIndicadorDel0 = Pr [Condicional (E [] 0) x  (E "E" 0) , 
                            Incremento y (E [] 0)]

-- λ> programaIndicadorDel0
--      IF X/=0 GOTO     
--      Y<-Y+1

-- Programa que calcula la función identidad
programaIdentidad :: Programa
programaIdentidad = Pr [Condicional (E [] 0) x (E "B" 0), 
                        Incremento z (E [] 0), Condicional (E [] 0) z (E "E" 0), 
                        Decremento x (E "B" 0), 
                        Incremento y (E [] 0), 
                        Condicional (E [] 0) x (E "B" 0)]


-- λ> programaIdentidad
--      IF X/=0 GOTO     
--      Z<-Z+1
--      IF Z/=0 GOTO     
-- [B]  X<-X-1
--      Y<-Y+1
--      IF X/=0 GOTO  
