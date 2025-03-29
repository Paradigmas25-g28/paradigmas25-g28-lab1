module Basica.BasicaDoble where
import Dibujo
import Interp
import Basica.Comun

data Basica = Triangulo | TrainguloVioleta

ejemplo :: Dibujo Basica
ejemplo = Apilar 1 1 (Basica Triangulo) (Rotar (Basica TrainguloVioleta))

interpBas :: Basica -> ImagenFlotante
interpBas Triangulo = triangulo
interpBas TrainguloVioleta = trianguloVioleta
