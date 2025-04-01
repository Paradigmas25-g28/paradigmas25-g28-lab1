module Basica.Ejemplo where
import Dibujo
import Interp
import Basica.Comun

type Basica = ()

ejemplo :: Dibujo Basica
--ejemplo = Espejar (Basica ())
--ejemplo = Apilar 1 1 (Espejar (Basica ())) (Espejar (Basica ()))
--ejemplo = Encimar (Basica()) (Rotar(Basica()))
--ejemplo = Juntar 1 1 (Basica()) (Rotar(Basica()))
ejemplo = Rot45 (Basica()) -- (corregir rot45)

interpBas :: Basica -> ImagenFlotante
interpBas () = formaF
