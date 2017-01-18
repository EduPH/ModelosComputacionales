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
--      Z2<-Z2-1
--      IF Z2/=0 GOTO     
-- [A3] IF X2/=0 GOTO [B3]
--      Z24<-Z24+1
--      IF Z24/=0 GOTO [C3]
-- [B3] X2<-X2-1
--      Z2<-Z2+1
--      Z14<-Z14+1
--      Z25<-Z25+1
--      IF Z25/=0 GOTO [A3]
-- [C3] IF Z14/=0 GOTO [D3]
--      Z26<-Z26+1
--      IF Z26/=0 GOTO [Q3]
-- [D3] Z14<-Z14-1
--      X2<-X2+1
--      Z27<-Z27+1
--      IF Z27/=0 GOTO [C3]
-- [B]  IF Z/=0 GOTO [A] 
--      Z28<-Z28+1
--      IF Z28/=0 GOTO [E4]
-- [A]  Z2<-Z2-1
--      Z1<-Z1-1
--      IF Z1/=0 GOTO     
-- [A5] IF X1/=0 GOTO [B5]
--      Z39<-Z39+1
--      IF Z39/=0 GOTO [C5]
-- [B5] X1<-X1-1
--      Z1<-Z1+1
--      Z29<-Z29+1
--      Z40<-Z40+1
--      IF Z40/=0 GOTO [A5]
-- [C5] IF Z29/=0 GOTO [D5]
--      Z41<-Z41+1
--      IF Z41/=0 GOTO [Q5]
-- [D5] Z29<-Z29-1
--      X1<-X1+1
--      Z42<-Z42+1
--      IF Z42/=0 GOTO [C5]
--      Z3<-Z3-1
--      IF Z3/=0 GOTO     
-- [A6] IF Y/=0 GOTO [B6]
--      Z53<-Z53+1
--      IF Z53/=0 GOTO [C6]
-- [B6] Y<-Y-1
--      Z3<-Z3+1
--      Z43<-Z43+1
--      Z54<-Z54+1
--      IF Z54/=0 GOTO [A6]
-- [C6] IF Z43/=0 GOTO [D6]
--      Z55<-Z55+1
--      IF Z55/=0 GOTO [Q6]
-- [D6] Z43<-Z43-1
--      Y<-Y+1
--      Z56<-Z56+1
--      IF Z56/=0 GOTO [C6]
-- [B2] IF Z3/=0 GOTO [A2]
--      Z57<-Z57+1
--      IF Z57/=0 GOTO [E9]
-- [A2] Z3<-Z3-1
--      Z1<-Z1+1
--      Z58<-Z58+1
--      IF Z58/=0 GOTO [B12]
-- [E2] Y<-Y-1
--      IF Y/=0 GOTO [E2]
-- [A13] IF Z1/=0 GOTO [B13]
--      Z69<-Z69+1
--      IF Z69/=0 GOTO [C13]
-- [B13] Z1<-Z1-1
--      Y<-Y+1
--      Z59<-Z59+1
--      Z70<-Z70+1
--      IF Z70/=0 GOTO [A13]
-- [C13] IF Z59/=0 GOTO [D13]
--      Z71<-Z71+1
--      IF Z71/=0 GOTO [Q13]
-- [D13] Z59<-Z59-1
--      Z1<-Z1+1
--      Z72<-Z72+1
--      IF Z72/=0 GOTO [C13]
--      Z73<-Z73+1
--      IF Z73/=0 GOTO [B14]

-- | Ejemplos
-- >>> let p =progM2prog (Pm [(valV y (VarWork [1]) (E "E" 2))])
-- >>> let vars = [(y,0),(VarWork [1],0),(VarWork [24],0),(VarWork [25],0),(VarWork [14],0),(VarWork [26],0),(VarWork [27],1)]
-- >>> ejecutaP p vars
-- (Y,0)
-- >>> let vars = [(y,0),(VarWork [1],4),(VarWork [24],0),(VarWork [25],0),(VarWork [14],0),(VarWork [26],0),(VarWork [27],1)]
-- >>> ejecutaP p vars
-- (Y,4)
