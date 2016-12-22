module ProgramasGOTO where
import GOTO
import Macros

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
producto = Pm [ valV (VarWork [2]) (VarIn [2]) (E "" 0),
                CondM (E "B" 0) z (E "A" 0),
                goto (E "" 0) (E "E" 0),
                DecM (VarWork [2]) (E "A" 0),
                valV (VarWork [1]) (VarIn [1]) (E "" 0),
                valV (VarWork [3]) y (E "" 0),
                CondM (E "B" 2) (VarWork [3]) (E "A" 2),
                goto (E "" 0) (E "E" 2),
                DecM (VarWork [3]) (E "A" 2),
                IncM (VarWork [1]) (E "" 0),
                goto (E "" 0) (E "B" 2),
                valV y (VarWork [1]) (E "E" 2),
                goto (E "" 0) (E "B" 0)]

-- | Ejemplos
-- >>> let p = progM2prog producto
-- >>> p
--      IF X2/=0 GOTO [B3]
--      Z4<-Z4+1
--      IF Z4/=0 GOTO [C3]
-- [B3] X2<-X2-1
--      Z6<-Z6+1
--      Z4<-Z4+1
--      Z4<-Z4+1
--      IF Z4/=0 GOTO     
-- [C3] IF Z4/=0 GOTO [D3]
--      Z4<-Z4+1
--      IF Z4/=0 GOTO [E3]
-- [D3] Z4<-Z4-1
--      X2<-X2+1
--      Z4<-Z4+1
--      IF Z4/=0 GOTO [C3]
-- [B]  IF Z/=0 GOTO [A] 
--      Z7<-Z7+1
--      IF Z7/=0 GOTO [E4]
-- [A]  Z2<-Z2-1
--      IF X1/=0 GOTO [B5]
--      Z8<-Z8+1
--      IF Z8/=0 GOTO [C5]
-- [B5] X1<-X1-1
--      Z9<-Z9+1
--      Z8<-Z8+1
--      Z8<-Z8+1
--      IF Z8/=0 GOTO     
-- [C5] IF Z8/=0 GOTO [D5]
--      Z8<-Z8+1
--      IF Z8/=0 GOTO [E5]
-- [D5] Z8<-Z8-1
--      X1<-X1+1
--      Z8<-Z8+1
--      IF Z8/=0 GOTO [C5]
--      IF Y/=0 GOTO [B6]
--      Z10<-Z10+1
--      IF Z10/=0 GOTO [C6]
-- [B6] Y<-Y-1
--      Z13<-Z13+1
--      Z10<-Z10+1
--      Z10<-Z10+1
--      IF Z10/=0 GOTO     
-- [C6] IF Z10/=0 GOTO [D6]
--      Z10<-Z10+1
--      IF Z10/=0 GOTO [E6]
-- [D6] Z10<-Z10-1
--      Y<-Y+1
--      Z10<-Z10+1
--      IF Z10/=0 GOTO [C6]
-- [B2] IF Z3/=0 GOTO [A2]
--      Z14<-Z14+1
--      IF Z14/=0 GOTO [E9]
-- [A2] Z3<-Z3-1
--      Z1<-Z1+1
--      Z15<-Z15+1
--      IF Z15/=0 GOTO [B12]
-- [E2] IF Z17/=0 GOTO [B13]
--      Z16<-Z16+1
--      IF Z16/=0 GOTO [C13]
-- [B13] Z17<-Z17-1
--      Y<-Y+1
--      Z16<-Z16+1
--      Z16<-Z16+1
--      IF Z16/=0 GOTO [E2]
-- [C13] IF Z16/=0 GOTO [D13]
--      Z16<-Z16+1
--      IF Z16/=0 GOTO [E13]
-- [D13] Z16<-Z16-1
--      Z17<-Z17+1
--      Z16<-Z16+1
--      IF Z16/=0 GOTO [C13]
--      Z18<-Z18+1
--      IF Z18/=0 GOTO [B14]
-- >>> variablesDe p
-- [X2,Z4,Z6,Z,Z7,Z2,X1,Z8,Z9,Y,Z10,Z13,Z3,Z14,Z1,Z15,Z17,Z16,Z18]
-- >>> let vs = variablesDe p
-- >>> let vs' = [(v,0) | v <- vs, not (esX v)]
-- >>> let vars = vs' ++ [(VarIn [1], 2), (VarIn [2], 4)]
-- >>> vars
-- [(Z4,0),(Z6,0),(Z,0),(Z7,0),(Z2,0),(Z8,0),(Z9,0),(Y,0),(Z10,0),(Z13,0),(Z3,0),(Z14,0),(Z1,0),(Z15,0),(Z17,0),(Z16,0),(Z18,0),(X1,2),(X2,4)]
