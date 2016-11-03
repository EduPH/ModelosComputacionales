module MacrosGOTO where
import GOTO
import ProgramasGOTO

-- Salto incondicional 
goTo :: Etiqueta -> [Instruccion]
goTo e = [Incremento z [], 
          Condicional [] z e]

-- Hacer una variable 
anula :: Variable -> Etiqueta -> [Instruccion]
anula v e= [Decremento v e, Condicional [] v e]
