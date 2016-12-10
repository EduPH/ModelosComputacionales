module Macros where
import GOTO

-- | Instrucciones con macros.

data InstM  =  IncM Variable Etiqueta
                  | DecM Variable Etiqueta
                  | CondM Etiqueta Variable Etiqueta
                  | Macro Etiqueta [InstM]
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
     show (Macro (E "" n) [i]) = show i
     show (Macro e@(E "" n) (i:is)) = 
               show i ++ "\n" ++ show (Macro e is)
     show (Macro e [i])    = show e ++ show i
     show (Macro e (i:is)) = show e ++ show i ++ "\n" ++ show (Macro e is)

-- | Programas con macros.

data ProgramaM = Pm [InstM]

-- | Representación de programas con macros.

instance Show ProgramaM where
    show (Pm [i])    = show i
    show (Pm (i:is)) = (show i) ++ "\n" ++ (show (Pm is))

-- | Definimos las macro Y<-X y GOTO "E".

valX :: Etiqueta -> InstM
valX e = Macro e [CondM (E [] 0) x (E "A" 0),
                  IncM z (E [] 0),
                  CondM (E [] 0) z (E "E" 0),
                  DecM x (E "A" 0),
                  IncM y (E [] 0),
                  CondM (E [] 0) x (E "A" 0)]


goto :: Etiqueta -> Etiqueta -> InstM
goto e1 e2 = Macro e1 [IncM z (E [] 0),
                       CondM (E [] 0) z e2]

-- | Macro V <- V'.

valV v v' e = Macro e [CondM (E "A" 0) v' (E "B" 0),
                       goto (E "" 0) (E "C" 0),
                       DecM v' (E "B" 0),
                       IncM v  (E "" 0),
                       IncM z  (E "" 0),
                       goto (E "" 0) (E "A" 0),
                       CondM (E "C" 0) z (E "D" 0),
                       goto (E "" 0) (E "E" 0),
                       DecM z  (E "D" 0),
                       IncM v' (E "" 0),
                       goto (E "" 0) (E "C" 0)] 
