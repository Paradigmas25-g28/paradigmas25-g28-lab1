{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo

-- Gloss provee el tipo Vector y Picture.
type ImagenFlotante = Vector -> Vector -> Vector -> Picture
type Interpretacion a = a -> ImagenFlotante

mitad :: Vector -> Vector
mitad = (0.5 V.*)

-- Interpretaciones de los constructores de Dibujo

--interpreta el operador de rotacion
interp_rotar :: ImagenFlotante -> ImagenFlotante
interp_rotar imgf d w h = imgf (d V.+ h) h ((-1) V.* w)

--interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante
interp_espejar imgf d w = imgf (d V.+ w) ((-1) V.* w)

--interpreta el operador de rotacion 45
interp_rotar45 :: ImagenFlotante -> ImagenFlotante
interp_rotar45 imgf d w h = imgf (d V.+ mitad (w V.+h)) (mitad (w V.+ h)) (mitad (h V.- w))

--interpreta el operador de apilar
interp_apilar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar n m imgf1 imgf2 d w h =
  pictures [
    imgf1 ( d V.+ mulSV (n / (n + m)) h) w (mulSV (m / (m + n)) h),
    imgf2 d w (mulSV (n / (n + m)) h)
  ]
--interpreta el operador de juntar
interp_juntar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_juntar n m imgf1 imgf2 d w h =
  pictures [
    imgf1 d (mulSV (m / (n + m)) w) h,
    imgf2 ( d V.+ mulSV (m / (n + m)) w) (mulSV (n / (n + m)) w) h
  ]
--interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_encimar imgf1 imgf2 a b c = pictures [imgf1 a b c, imgf2 a b c]

--interpreta cualquier expresion del tipo Dibujo a
--utilizar foldDib 
interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp inter = foldDib inter
                       interp_rotar
                       interp_espejar
                       interp_rotar45
                       interp_apilar
                       interp_juntar 
                       interp_encimar
