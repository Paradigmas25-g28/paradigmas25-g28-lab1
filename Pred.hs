module Pred where

import Dibujo

type Pred a = a -> Bool

-- Para la definiciones de la funciones de este modulo, no pueden utilizar
-- pattern-matching, sino alto orden a traves de la funcion foldDib, mapDib

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por el resultado de llamar a la función indicada por el
-- segundo argumento con dicha figura.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Basica x))` rota
-- todos los triángulos.
-- A REVISAR FUNCION CAMBIAR ERROR DE TIPOS
cambiar :: Pred a -> (a -> Dibujo a) -> Dibujo a -> Dibujo a
cambiar p f = foldDib (\x -> if p x then f x else Basica x) rotar rotar45 espejar apilar juntar encimar

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib pred = foldDib pred id id id (\_ _ d1 d2 -> d1 || d2) (\_ _ d1 d2 -> d1 || d2) (\d1 d2 -> d1 || d2)

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib pred = foldDib pred id id id (\_ _ d1 d2 -> d1 && d2) (\_ _ d1 d2 -> d1 && d2) (\d1 d2 -> d1 && d2)

-- Hay 4 rotaciones seguidas.
esRot360 :: Pred (Dibujo a)
esRot360 d =
  foldDib
    (\x -> 0)
    (\x -> if x < 4 then x + 1 else 4)
    (\x -> if x == 4 then 4 else 0)
    (\x -> if x == 4 then 4 else 0)
    (\_ _ x y -> if x == 4 || y == 4 then 4 else 0)
    (\_ _ x y -> if x == 4 || y == 4 then 4 else 0)
    (\x y -> if x == 4 || y == 4 then 4 else 0)
    d
    == 4

-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a)
esFlip2 d =
  foldDib
    (\x -> 0)
    (\x -> if x == 2 then 2 else 0)
    (\x -> if x < 2 then x + 1 else 2)
    (\x -> if x == 2 then 2 else 0)
    (\_ _ x y -> if x == 2 || y == 2 then 2 else 0)
    (\_ _ x y -> if x == 2 || y == 2 then 2 else 0)
    (\x y -> if x == 2 || y == 2 then 2 else 0)
    d
    == 2

data Superfluo = RotacionSuperflua | FlipSuperfluo
  deriving (Show, Eq)

---- Chequea si el dibujo tiene una rotacion superflua
errorRotacion :: Dibujo a -> [Superfluo]
errorRotacion d = [RotacionSuperflua | esRot360 d]

-- Chequea si el dibujo tiene un flip superfluo
errorFlip :: Dibujo a -> [Superfluo]
errorFlip d = [FlipSuperfluo | esFlip2 d]

-- Aplica todos los chequeos y acumula todos los errores, y
-- sólo devuelve la figura si no hubo ningún error.
checkSuperfluo :: Dibujo a -> Either [Superfluo] (Dibujo a)
checkSuperfluo d =
  let errores = errorRotacion d ++ errorFlip d
   in if null errores
        then Right d
        else Left errores

-- Ejemplos para probar pred:
dibujoRot4 = Rotar (Rotar (Rotar (Rotar (Basica "X"))))

-- True
dibujoRot3 = Rotar (Rotar (Rotar (Basica "X")))

-- Flase
type Figura = String

-- Dibujo original
ej1 :: Dibujo Figura
ej1 = encimar (Basica "A") (Basica "C")

-- Cambiar todas las "A" por una Rotar "B"
ej1Modificado = cambiar (== "A") (\_ -> rotar (Basica "B")) ej1

-- Dibujo con varias figuras
ej2 = juntar 1 1 (Basica "A") (Basica "B")

-- ¿Alguna figura es "B"?
hayB = anyDib (== "B") ej2 -- True

-- ¿Alguna figura es "Z"?
hayZ = anyDib (== "Z") ej2 -- False
-- Dibujo sólo con "A"

ej3 = apilar 1 1 (Basica "A") (Basica "A")

-- ¿Todas las figuras son "A"?
todasA = allDib (== "A") ej3 -- True

-- ¿Todas las figuras son "B"?
todasB = allDib (== "B") ej3 -- False

-- 2 espejados
ej5 = Espejar (Espejar (Basica "A"))

-- 1 espejado
ej5b = Espejar (Basica "A")

-- Resultados
testFlip2_True = esFlip2 ej5 -- True

testFlip2_False = esFlip2 ej5b -- False

-- Dibujo con rotación superflua
ej6 = Rotar (Rotar (Rotar (Rotar (Basica "A"))))

-- Dibujo con doble flip
ej7 = Espejar (Espejar (Basica "A"))

-- Dibujo sin errores
ej8 = Rotar (Basica "A")

-- checkSuperfluo ej6 -- Left [RotacionSuperflua]
-- checkSuperfluo ej7 -- Left [FlipSuperfluo]
-- checkSuperfluo ej8 -- Right (Rotar (Basica "A"))
