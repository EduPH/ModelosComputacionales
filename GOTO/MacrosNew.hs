module MacrosNew where
import GOTO
import Data.List 

-- * Implementación Macros GOTO
-- ============================

-- | Instrucciones con macros.

data InstM  =  IncM Variable Etiqueta
                  | DecM Variable Etiqueta
                  | CondM Etiqueta Variable Etiqueta
                  | Macro Etiqueta [Variable] [InstM]
                  | SKIPM Etiqueta
                    deriving Eq

-- | Representación por pantalla. 

instance Show InstM where
     show (IncM v l)     = show l ++ " " ++ show v ++ "<-" 
                                 ++ show v ++"+" ++ (show 1)
     show (DecM v l)     = show l ++ " "  ++show v ++ "<-" 
                                 ++ show v ++"-" ++ (show 1)
     show (CondM l v l') = show l ++ " " ++ "IF" ++ " "  
                                 ++ show v ++ "/=" ++ (show 0)
                                 ++" "++ "GOTO" ++" " ++ show l'
     show (SKIPM l)      = show l ++ " " ++ "Y <- Y"
     show (Macro (E "" n) vs [i]) = show i
     show (Macro e@(E "" n) vs (i:is)) = 
               show i ++ "\n" ++ show (Macro e vs is)
     show (Macro e vs [i])    = show e ++ show i
     show (Macro e vs (i:is)) = show e ++ show i ++ "\n" ++ 
                                show (Macro e vs is)

-- | Ejemplos
-- >>> IncM x (E "A" 0)
-- [A]  X<-X+1
-- >>> DecM y (E "B" 4)
-- [B4] Y<-Y-1
-- >>> CondM (E "" 0) x  (E "A" 0)
--      IF X/=0 GOTO [A] 
-- >>> Macro (E "A" 0) [] [IncM x (E "" 0), DecM x (E "B" 0), CondM (E "" 0) z (E "B" 3)]
-- [A]      X<-X+1
-- [A] [B]  X<-X-1
-- [A]      IF Z/=0 GOTO [B3]

-- | Programas con macros.

data ProgramaM = Pm [InstM]

-- | Representación de programas con macros.

instance Show ProgramaM where
    show (Pm [i])    = show i
    show (Pm (i:is)) = (show i) ++ "\n" ++ (show (Pm is))

-- | Definimos las macros Y<-X y GOTO "E".

valX :: Etiqueta -> InstM
valX e = Macro e  [] [CondM (E [] 0) x (E "A" 0),
                      IncM z (E [] 0),
                      CondM (E [] 0) z (E "E" 0),
                      DecM x (E "A" 0),
                      IncM y (E [] 0),
                      CondM (E [] 0) x (E "A" 0)]


goto :: Etiqueta -> Etiqueta -> InstM
goto e1 e2 = Macro e1 [] [IncM z (E [] 0),
                          CondM (E [] 0) z e2]

-- | Macro V <- V'.

valV v v' e = Macro e [v,v'] [DecM v (E "A" 1),
                              CondM (E "" 0) v (E "A" 1),
                              CondM (E "A" 0) v' (E "B" 0),
                              IncM (VarWork [10]) (E [] 0),
                              CondM (E [] 0) (VarWork [10]) (E "C" 0),
                              DecM v' (E "B" 0),
                              IncM v  (E "" 0),
                              IncM (VarWork [14])  (E "" 0),
                              IncM (VarWork [11]) (E [] 0),
                              CondM (E [] 0) (VarWork [11]) (E "A" 0),
                              CondM (E "C" 0) (VarWork [14]) (E "D" 0),
                              IncM (VarWork [12]) (E [] 0),
                              CondM (E [] 0) (VarWork [12]) (E "Q" 0),
                              DecM (VarWork [14])  (E "D" 0),
                              IncM v' (E "" 0),
                              IncM (VarWork [13]) (E [] 0),
                              CondM (E [] 0) (VarWork [13]) (E "C" 0),
                              IncM (VarWork [15]) (E "Q" 0)] 

-----------------------------------------------------------------------
--
-- * Normalización de macros
-- =========================

-- ** Normalización de índices de variables de trabajo
-- ==================================================

-- | La función (susVar v v1 i) sustituye la variable v1 por v en la
-- instrucción i. 

susVar :: Variable -> Variable -> InstM -> InstM
susVar v1 v i | v1 == v = i
susVar v1 v i@(IncM v' e) | v' == v1 = IncM v e
                          | otherwise = i
susVar v1 v i@(DecM v' e) | v' == v1 = DecM v e
                          | otherwise = i
susVar v1 v i@(CondM e v' e') | v' == v1 =  CondM e v e'
                              | otherwise = i
susVar v1 v (Macro e vs is) = Macro e vs (aux is)
    where
      aux [i] = [susVar v1 v i]
      aux (i:is) = (susVar v1 v i): aux is
susVar v1 v (SKIPM e) = SKIPM e
-- | Ejemplos
-- >>> IncM x (E "" 0)
--      X<-X+1
-- >>> susVar x (VarIn [2]) (IncM x (E "" 0))
--      X2<-X2+1

-- | La función (varsInstM i) calcula la lista de las variables de la
-- instrucción i, si es una macro, no aparecerán aquellas de la lista de
-- variables de trabajo auxiliares. 

varsInstM :: InstM -> [Variable]
varsInstM (IncM v _) = [v]
varsInstM (DecM v _) = [v]
varsInstM (CondM e v e') = [v]
varsInstM (Macro e vs is) = nub (concat ( map (varsInstM ) is) ) \\ vs
varsInstM (SKIPM e) = []

-- | Ejemplos
-- >>> varsInstM (IncM x (E "" 0))
-- [X]
-- >>> varsInstM (valX (E "" 0))
-- [X,Z,Y]

-- | La función (varTrab is) calcula la lista de las variables de
-- trabajo de una lista de instrucciones. 

varTrab :: [InstM] -> [Variable]
varTrab is = nub [ v | v <- vs, esZ v]
    where
      vs = concat (map (varsInstM) is)

-- | La función (paresVars n vs) calcula a partir de una lista de
-- variables de trabajo vs, pares formados por la variable y su
-- normalizada. 

paresVars :: Int -> [Variable] -> [(Variable, Variable)]
paresVars n vs = [(v,aux  v) | v <- vs]
    where
      aux v = VarWork [indice v +n]

-- | Ejemplos
-- >>> paresVars 5 [z,VarWork [2], VarWork [6]]
-- [(Z,Z5),(Z2,Z7),(Z6,Z11)]

-- | La función (normInd n is vs) normaliza las variables de las
-- instrucciones de is mediante los pares de vs, según el entero n. 

normInd :: Int -> [InstM] -> [(Variable, Variable)] -> [InstM]
normInd n is [] = is
normInd n is (v:vs) = normInd n [susVar (fst v) (snd v) i | i<- is] vs

-- | Ejemplos   
-- >>> normInd 5 [IncM z (E "" 0), DecM z (E "" 0), Macro (E "" 0) [] [IncM z (E "B" 0), DecM z (E "" 0)]] [(z, VarWork [2])]
-- [     Z2<-Z2+1,     Z2<-Z2-1,[B]  Z2<-Z2+1
--      Z2<-Z2-1]
 
-- | La función (normalizaIndices n macro) normaliza los índices de las
-- variables de la macro según un entero n. 

normalizaIndices :: Int -> InstM -> InstM
normalizaIndices n (Macro e vs' is) = Macro e vs' (normInd n is vs)
    where
      vs = paresVars n ((varTrab is) \\ vs')

-- | Ejemplos
-- >>> normalizaIndices 5 (valX (E "" 0))
--      IF X/=0 GOTO [A] 
--      Z5<-Z5+1
--      IF Z5/=0 GOTO [E] 
-- [A]  X<-X-1
--      Y<-Y+1
--      IF X/=0 GOTO [A] 
-- >>> normalizaIndices 5 (valV  z (VarWork [2]) (E "" 0))
-- [A]  IF Z2/=0 GOTO [B] 
--      Z<-Z+1
--      IF Z/=0 GOTO [C] 
-- [B]  Z2<-Z2-1
--      Z<-Z+1
--      Z<-Z+1
--      Z<-Z+1
--      IF Z/=0 GOTO [A] 
-- [C]  IF Z/=0 GOTO [D] 
--      Z<-Z+1
--      IF Z/=0 GOTO [E] 
-- [D]  Z<-Z-1
--      Z2<-Z2+1
--      Z<-Z+1
--      IF Z/=0 GOTO [C] 

-- | La función (esM m) determina si m es una macro.

esM :: InstM -> Bool
esM (Macro _ _ _) = True
esM _ = False

-- | La función (maximoInd is) calcula el índice mayor de las variables
-- de trabajo de la lista de instrucciones is.

maximoInd :: [InstM] -> Int
maximoInd is = maximum [indice v | v <- varTrab is]

-- | La función (normalizaIndPm pm) normaliza los índices de las variables
-- de trabajo de las macros del programa pm. 

normalizaIndPm :: ProgramaM -> ProgramaM
normalizaIndPm (Pm is) = Pm (aux n is)
    where
      n = maximoInd is +1
      aux n [] = [] 
      aux n (i:is) | esM i = (normalizaIndices n i): 
                             (aux (maximoInd [normalizaIndices n i]+1) is)
                   | otherwise = i: (aux n is)

-- ** Normalización etiquetas
-- =========================

-- | La función (etiquetaM i) devuelve la lista de las etiquetas de la
-- instrucción i. 

etiquetaM :: InstM -> [Etiqueta]
etiquetaM (IncM _ e) = [e]
etiquetaM (DecM _ e) = [e]
etiquetaM (CondM e1 _ e2) = [e1,e2]
etiquetaM (SKIPM e) = [e]
etiquetaM (Macro _ _ is) = concat (map (etiquetaM) is)

-- | La función (indexEt e) devuelve el índice de la etiqueta e.

indexEt :: Etiqueta -> Int
indexEt (E _ n) = n

-- | La función (maxIndexEt es) calcula el máximo índice de las
-- etiquetas de una listas de etiquetas. 

maxIndexEt :: [Etiqueta] -> Int
maxIndexEt es = maximum [indexEt e | e <- es]


-- | La función (susEt e e' i) sustituye la etiqueta e por e' en la
-- instrucción i. 

susEt :: Etiqueta -> Etiqueta -> InstM -> InstM
susEt e e' i | e == e' = i
susEt e e' i@(IncM v e1)  | e1 == e = IncM v e'
                          | otherwise = i
susEt e e' i@(DecM v e1)  | e1 == e = DecM v e'
                          | otherwise = i
susEt e e' i@(CondM e1 v e2) | e1 == e = susEt e e' (CondM e' v e2)
                             | e2 == e = susEt e e' (CondM e1 v e')
                             | otherwise = i
susEt e e' (Macro e1 vs is) = Macro e1 vs (aux is)
    where
      aux [i] = [susEt e e' i]
      aux (i:is) = (susEt e e' i): aux is
susEt e e' (SKIPM e1) | e1 == e = SKIPM e'
                      | otherwise = SKIPM e1
-- | La función (paresEt n es) calcula a partir de una lista de
-- etiquetas es, pares formados por la etiqueta y su normalizada. 

paresEt :: Int -> [Etiqueta] -> [(Etiqueta,Etiqueta)]
paresEt n es = [(e, aux e) | e <- es]
    where
      aux (E str n') = E str (n'+n)

-- | La función (normEt n is es) normaliza las etiquetas de las
-- instrucciones de is mediante los pares de es, según el entero n. 

normEt :: Int -> [InstM] -> [(Etiqueta, Etiqueta)] -> [InstM]
normEt n is [] = is
normEt n is (e:es) = normEt n [susEt (fst e) (snd e) i | i <- is] es

-- | La función (normalizaIndicesEt n i)  es la normalización de los
-- índices de las etiquetas de i.

normalizaIndicesEt :: Int -> InstM -> InstM
normalizaIndicesEt n (Macro e vs is) = Macro e vs (normEt n is es)
    where
      es = paresEt n (concat (map (etiquetaM) is))

-- | La función (normEtPm pm) normaliza las etiquetas del programa pm.

normEtPm :: ProgramaM -> ProgramaM
normEtPm (Pm is) = Pm (aux n is)
    where
      n = maxIndexEt (concat (map (etiquetaM) is))+1
      aux n [] = [] 
      aux n (i:is) 
          | esM i = (normalizaIndicesEt n i): 
                    (aux (maxIndexEt 
                          (concat [etiquetaM 
                                   (normalizaIndicesEt n i)])+1) is)
          | otherwise = i: (aux n is)

-- | La función (normM pm) genera la expansión normalizada del programa
-- pm.

normM :: ProgramaM -> ProgramaM
normM = normEtPm . normalizaIndPm


-- | Ejemplos
-- >>> let p =Pm [IncM z (E "" 0), valV x z (E "A" 0), DecM x (E "B" 0)]
-- >>> normM p
--      Z<-Z+1
-- [A] [A1] IF Z1/=0 GOTO [B1]
-- [A]      Z1<-Z1+1
--      IF Z1/=0 GOTO [C1]
-- [A] [B1] Z1<-Z1-1
-- [A]      X<-X+1
-- [A]      Z1<-Z1+1
-- [A]      Z1<-Z1+1
--      IF Z1/=0 GOTO [A1]
-- [A] [C1] IF Z1/=0 GOTO [D1]
-- [A]      Z1<-Z1+1
--      IF Z1/=0 GOTO [E1]
-- [A] [D1] Z1<-Z1-1
-- [A]      Z1<-Z1+1
-- [A]      Z1<-Z1+1
--      IF Z1/=0 GOTO [C1]
-- [B]  X<-X-1

-- | La función (noEsVacia e) determina si la etiqueta e tiene nombre.

noEsVacia :: Etiqueta -> Bool 
noEsVacia (E [] _) = False
noEsVacia _ = True

-- | La función (normEtMAux m) genera una macro cuya primera instrucción tiene la misma etiqueta que la macro.

normEtMAux :: InstM -> InstM
normEtMAux (Macro e vs ins@(i:is)) 
    | not (noEsVacia e) = Macro e vs ins
    | noEsVacia (head (etiquetaM i)) && noEsVacia e =
       Macro e vs (map (susEt (head (etiquetaM i)) e) ins )
    | otherwise = Macro e vs ((susEt (head (etiquetaM i)) e i):is)
normEtMAux i = i

-- | La función (instDeMacro m) obtiene la lista de instrucciones de la macro m.

instDeMacro :: InstM -> [InstM]
instDeMacro (Macro _ _ v) = v

-- | La función (instM2inst i) convierte instrucciones tipo macro a
-- instrucciones.

instM2inst :: InstM -> [Instruccion]
instM2inst (IncM v e) = [Incremento v e]
instM2inst (DecM v e) = [Decremento v e]
instM2inst (CondM e v e') = [Condicional e v e']
instM2inst (SKIPM e) = [SKIP e]
instM2inst m@(Macro e vs v) = 
    concat (map (instM2inst) (instDeMacro (normEtMAux m)))

-- | La función (progM2progAux p) convierte programas con macros a
-- programas sin macros.

progM2progAux :: ProgramaM -> Programa
progM2progAux (Pm is) = Pr (concat (map (instM2inst) is))

-- | La función (progM2prog p) convierte programas con macros a
-- programas sin macros, con normalización previa. 

progM2prog :: ProgramaM -> Programa
progM2prog = progM2progAux . normM 

-- | Ejemplo
-- >>> let p =Pm [IncM z (E "" 0), valV x z (E "A" 0), DecM x (E "B" 0)]
-- >>> progM2prog p
--      Z<-Z+1
-- [A]  IF Z1/=0 GOTO [B1]
--      Z1<-Z1+1
--      IF Z1/=0 GOTO [C1]
-- [B1] Z1<-Z1-1
--      X<-X+1
--      Z1<-Z1+1
--      Z1<-Z1+1
--      IF Z1/=0 GOTO [A] 
-- [C1] IF Z1/=0 GOTO [D1]
--      Z1<-Z1+1
--      IF Z1/=0 GOTO [E1]
-- [D1] Z1<-Z1-1
--      Z1<-Z1+1
--      Z1<-Z1+1
--      IF Z1/=0 GOTO [C1]
-- [B]  X<-X-1
-- >>> let p = Pm [Macro (E "" 0) []  [IncM x (E "" 0), Macro (E "" 0) [] [IncM x (E "" 0), Macro (E "" 0) [] [IncM x (E "" 0)]]]]
-- >>> progM2prog p
--      X<-X+1
--      X<-X+1
--      X<-X+1

-- | Ejemplo
-- >>> let p = progM2prog (Pm [valV z x (E "" 0)  ,valV y z (E "" 0) ])
-- >>> p
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
-- >>> ejecutaP p [(z,0),(x,7),(VarWork [26],0),(VarWork [30],0),(VarWork [27],0),(VarWork [28],0),(VarWork [29],0),(VarWork [31],0),(VarWork [42],0),(VarWork [46],0),(VarWork [43],0),(VarWork [44],0),(VarWork [45],0),(VarWork [47],0),(y,0)]
-- (Y,7)
