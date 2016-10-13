module Programas where
import GOTO

-- Programa lineal que devuelve la constante 3 para cualquier variable de entrada
programaCte3 = Pr [Incremento y [], Incremento y [], Incremento y []]

-- Programa que devuelve 1 si X=0, y devuelve 0 en caso contrario
programaIndicadorDel0 = Pr [Condicional [] x "E", Incremento y []]


-- Programa que calcula la función identidad:
programaIdentidad :: Programa
programaIdentidad = Pr [Condicional [] x "B", 
                        Incremento z [], Condicional [] z "E", 
                        Decremento x "B", 
                        Incremento y [], 
                        Condicional [] x "B"]
