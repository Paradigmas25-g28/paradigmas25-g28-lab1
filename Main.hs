module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.UI.GLUT.Begin
import Dibujo
import Interp
import qualified Basica.Ejemplo as E
import qualified Graphics.Gloss.Data.Point.Arithmetic as V
import qualified Basica.Escher as Es

--Funciones para rellenar el fondo de la imagen inicial

-- comprender esta función es un buen ejericio.
lineasH :: Vector -> Float -> Float -> [Picture]
lineasH origen@(x, y) longitud separacion = map (lineaH . (*separacion)) [0..]
  where lineaH h = line [(x, y + h), (x + longitud, y + h)]

-- Una grilla de n líneas, comenzando en origen con una separación de sep y
-- una longitud de l (usamos composición para no aplicar este
-- argumento)
grilla :: Int -> Vector -> Float -> Float -> Picture
grilla n origen sep l = pictures [ls, lsV]
  where ls = pictures $ take (n+1) $ lineasH origen sep l
        lsV = translate 0 (l*toEnum n) (rotate 90 ls)

-- Configuración para interpretar un dibujo
data Conf a = Conf {
    basic :: a -> ImagenFlotante
  , fig  :: Dibujo a
  , width :: Float
  , height :: Float
  , r :: Picture -> Picture  -- Reposicionar figura
  }

ej ancho alto = Conf {
                basic = Es.interpBas -- or E.interpBas
              , fig = Es.escher 3 True -- or E.ejemplo
              , width = ancho
              , height = alto
              , r = moverCentro ancho alto
              }

moverCentro :: Float -> Float -> Picture -> Picture
moverCentro ancho alto p = translate (-ancho / 2) (-alto / 2) p

ejCentro ancho alto = Conf {
                basic = Es.interpBas -- or E.interpBas
              , fig = Es.escher 3 True -- or E.ejemplo
              , width = ancho
              , height = alto
              , r = moverCentro ancho alto
              }

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
-- inicial :: IO (Conf a) -> IO () -- or (Conf E.Basica)
-- inicial cf = cf >>= \cfg ->
--     let ancho  = (width cfg, 0)
--         alto  = (0, height cfg)
--         imagen = interp (basic cfg) (fig cfg) (-5, -5) ancho alto
--     in display win white . withGrid $ imagen
--   where grillaGris = color grey $ grilla 10 (0, 0) 100 10
--         withGrid p = pictures [p, grillaGris]
--         grey = makeColorI 120 120 120 120
animar :: IO (Conf a) -> IO ()
animar cf = cf >>= \cfg ->
  animate win white (frame cfg)

frame :: Conf a -> Float -> Picture
frame cfg t =
  let
    ciclo = 6.28  -- 2 * pi, una vuelta
    tiempoTotal = 6.00  -- en segundos

    -- fase 1: girar en círculo
    -- fase 2: volver al centro suavemente
    (dx, dy)
      | t <= tiempoTotal =
          let angulo = 2 * pi * (t / tiempoTotal)
              r = 100  -- radio del círculo
          in ((r * cos angulo) - 5, (r * sin angulo) - 5)
      | t <= tiempoTotal * 2 =
          let tiempoRelativo = (t - tiempoTotal) / tiempoTotal
              angulo = 2 * pi
              r = 100 * (1 - tiempoRelativo)
          in ((r * cos angulo) - 5, (r * sin angulo) - 5)
      | otherwise = (-5, -5)

    origen = (dx, dy)
    ancho  = (width cfg, 0)
    alto   = (0, height cfg)
    imagen = interp (basic cfg) (fig cfg) origen ancho alto

    gris = makeColorI 120 120 120 120
    grillaGris = translate (-width cfg / 2) (-height cfg / 2) $
                 color gris (grilla 10 (0, 0) 100 10)
  in pictures [r cfg imagen, grillaGris]

win = InWindow "Paradigmas 2025 - Lab1" (500, 500) (0, 0)
main = animar $ return (ej 110 110)



