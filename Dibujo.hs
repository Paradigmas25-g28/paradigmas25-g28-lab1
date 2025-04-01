module Dibujo where

-- Definir el lenguaje via constructores de tipo
data Dibujo a = Basica a
               | Rotar (Dibujo a)
               | Espejar (Dibujo a)
               | Rot45 (Dibujo a)
               | Apilar Float Float (Dibujo a) (Dibujo a)
               | Juntar Float Float (Dibujo a) (Dibujo a)
               | Encimar (Dibujo a) (Dibujo a)
                deriving (Show, Eq)

rotar :: Dibujo a -> Dibujo a
rotar = Rotar

rotar45 :: Dibujo a -> Dibujo a
rotar45 = Rot45

espejar :: Dibujo a -> Dibujo a
espejar = Espejar

apilar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
apilar = Apilar

juntar :: Float -> Float -> Dibujo a -> Dibujo a -> Dibujo a
juntar = Juntar

encimar :: Dibujo a -> Dibujo a -> Dibujo a
encimar = Encimar

-- Composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp f 0 x = x
comp f n x = f (comp f (n-1) x)

-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 = comp Rotar 2 

r270 :: Dibujo a -> Dibujo a
r270 = comp Rotar 3 

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = Apilar 1 1


-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = Juntar 1 1


-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = Encimar

-- Dadas cuatro dibujos las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto a b c d = (///) ((.-.) a b) ((.-.) c d)

-- Una dibujo repetido con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 d = (^^^) d ((^^^) (r180 d) ((^^^) (Rotar d) (r270 d)))

-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar d = cuarteto d (Rotar d) (r180 d) (r270 d)

-- Transfomar un valor de tipo a como una Basica.
pureDib :: a -> Dibujo a
pureDib = Basica
-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Basica a) = Basica (f a)
mapDib f (Rotar d) = Rotar (mapDib f d)
mapDib f (Espejar d) = Espejar (mapDib f d)
mapDib f (Rot45 d) = Rot45 (mapDib f d)
mapDib f (Apilar x y d1 d2) = Apilar x y (mapDib f d1) (mapDib f d2)
mapDib f (Juntar x y d1 d2) = Juntar x y (mapDib f d1) (mapDib f d2)
mapDib f (Encimar d1 d2) = Encimar (mapDib f d1) (mapDib f d2)
-- Funcion de fold para Dibujos a
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) ->
       (Float -> Float -> b -> b -> b) ->
       (b -> b -> b) ->
       Dibujo a -> b
foldDib fBas fRot fEsp fRot45 fAp fJun fEnc (Basica a) = fBas a
foldDib fBas fRot fEsp fRot45 fAp fJun fEnc (Rotar d) = fRot (foldDib fBas fRot fEsp fRot45 fAp fJun fEnc d)
foldDib fBas fRot fEsp fRot45 fAp fJun fEnc (Espejar d) = fEsp (foldDib fBas fRot fEsp fRot45 fAp fJun fEnc d)
foldDib fBas fRot fEsp fRot45 fAp fJun fEnc (Rot45 d) = fRot45 (foldDib fBas fRot fEsp fRot45 fAp fJun fEnc d)
foldDib fBas fRot fEsp fRot45 fAp fJun fEnc (Apilar x y d1 d2) = fAp x y (foldDib fBas fRot fEsp fRot45 fAp fJun fEnc d1) (foldDib fBas fRot fEsp fRot45 fAp fJun fEnc d2)
foldDib fBas fRot fEsp fRot45 fAp fJun fEnc (Juntar x y d1 d2) = fJun x y (foldDib fBas fRot fEsp fRot45 fAp fJun fEnc d1) (foldDib fBas fRot fEsp fRot45 fAp fJun fEnc d2)
foldDib fBas fRot fEsp fRot45 fAp fJun fEnc (Encimar d1 d2) = fEnc (foldDib fBas fRot fEsp fRot45 fAp fJun fEnc d1) (foldDib fBas fRot fEsp fRot45 fAp fJun fEnc d2)




