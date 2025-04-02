module Basica.Escher where

import Basica.Comun
import Dibujo
import Graphics.Gloss
import Interp

-- supongamos que eligen
type Escher = Bool

-- el dibujo u
dibujoU :: Dibujo Escher -> Dibujo Escher
dibujoU p = encimar4 $ Rot45 p

-- el dibujo t
dibujoT :: Dibujo Escher -> Dibujo Escher
dibujoT d =
  let r1 = Rot45 d
      r2 = Espejar $ Rotar (Rot45 d)
   in (d ^^^ r1) ^^^ r2

-- lado con nivel de detalle
-- lado :: Int -> Dibujo Escher -> Dibujo Escher
-- lado n d
--   | n < 0 = Basica False -- no deberia pasar
--   | n == 1 = cuarteto (Basica False) (Basica False) (Rotar d) d
--   | n > 1 =
--       let l = lado (n - 1) d
--        in cuarteto l l (Rotar d) d
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 0 _ = Basica False
lado n p = cuarteto (lado (n - 1) p) (lado (n - 1) p) (Rotar (dibujoT p)) (dibujoT p)

-- esquina con nivel de detalle en base a la figura p
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina n d
  | n < 0 = Basica False -- no deberia pasar
  | n == 1 = cuarteto (Basica False) (Basica False) (Basica False) (dibujoU d)
  | n > 1 =
      let l = lado (n - 1) d
       in cuarteto (esquina (n - 1) d) l (Rotar l) (dibujoU d)

-- por suerte no tenemos que poner el tipo!
-- noneto p q r s t u v w x =
--   let pq = Juntar 2 1 p q
--       pqr = Juntar 1 3 pq r
--       st = Juntar 2 1 s t
--       stu = Juntar 1 3 st u
--       vw = Juntar 2 1 v w
--       vwx = Juntar 1 3 vw x
--    in Apilar 1 3 (Apilar 2 1 pqr stu) vwx
noneto p q r s t u v w x =
  Apilar
    1
    2
    (Juntar 1 2 p (Juntar 1 1 q r))
    (Apilar 1 1 (Juntar 1 2 s (Juntar 1 1 t u)) (Juntar 1 2 v (Juntar 1 1 w x)))

-- el dibujo de Escher:
escher :: Int -> Escher -> Dibujo Escher
escher n e =
  let p = esquina n $ pureDib e
      q = lado n $ pureDib e
      r = r270 p
      s = Rotar q
      t = dibujoU $ pureDib e
      u = r270 q
      v = Rotar p
      w = r180 q
      x = r180 p
   in noneto p q r s t u v w x

interpBas :: Escher -> ImagenFlotante
interpBas True = trianguloBorde
interpBas False = nada
