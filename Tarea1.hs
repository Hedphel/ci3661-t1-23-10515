-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Samuel José Piña González
--  Carnet: 23-10515
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
quitarAcentoChar :: Char -> Char
quitarAcentoChar c
  | c == 'á' || c == 'ä' || c == 'Á' || c == 'Ä' = 'a'
  | c == 'é' || c == 'ë' || c == 'É' || c == 'Ë' = 'e'
  | c == 'í' || c == 'ï' || c == 'Í' || c == 'Ï' = 'i'
  | c == 'ó' || c == 'ö' || c == 'Ó' || c == 'Ö' = 'o'
  | c == 'ú' || c == 'ü' || c == 'Ú' || c == 'Ü' = 'u'
  | otherwise = c

limpiarCadena :: String -> String
limpiarCadena [] = []
limpiarCadena (x:xs)
  | isAlphaNum charSinAcc = toLower charSinAcc : limpiarCadena xs
  | otherwise = limpiarCadena xs
  where charSinAcc = quitarAcentoChar x

reversa :: String -> String
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

esPalindromo :: String -> Bool
esPalindromo s = cadenaLimpia == reversa cadenaLimpia
  where cadenaLimpia = limpiarCadena s

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
-- Hacemos un llamado recursivo con una función auxiliar llamada aux, y en lo
-- que recorremos la lista, verificamos si cada elemento es mayor a nuestro máx
-- o si es menor a nuestro mín, y vamos actualizando hasta el final de la lista
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:xs) = Just (aux x x xs)
  where 
    -- Aquí definimos una función auxiliar interna
    aux min max [] = (min,max) -- Lista vacía
    aux min max (y:ys)
     | y < min = aux y max ys -- Aquí reemplazamos el valor mín por y
     | y > max = aux min y ys -- y aquí reemplazamos x
     | otherwise = aux min max ys

-- A3
esDigito :: Char -> Bool
esDigito c = c >= '0' && c <= '9'

leerHora :: String -> Either String (Int, Int)
leerHora [a, b, ':', c, d]
  | not (esDigito a && esDigito b && esDigito c && esDigito c) = Left "formato inválido"
  | horas >= 24 = Left "hora fuera de rango"
  | mins  >= 60 = Left "minutos fuera de rango"
  | otherwise   = Right(horas,mins)
  where
    horas = digitToInt a*10 + digitToInt b
    mins = digitToInt c*10 + digitToInt d

leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion str1 str2 = 
  case leerHora str1 of
    -- Si tenemos un fallo en la duracion 1, devolvemos su error
    Left err1 -> Left err1 
    Right (h1, m1) -> 
      case leerHora str2 of
        -- Si pasa la primera pero falla la 2da, devolvemos este ultimo error
        Left err2 -> Left err2
        Right (h2, m2) ->
          -- Buscamos ver la diferencia de minutos
          let total1 = h1*60 + m1
              total2 = h2*60 + m2
              dif = total2 - total1
          -- Si la diferencia es negativa, estamos yendo al próximo día
          -- Agregamos 60*24 = 1440 minutos.
          in if dif < 0
            then Right (dif + 1440)
            else Right dif

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
-- promedioSobreUmbral :: Double -> [Double] -> Maybe Double
-- promedioSobreUmbral xs umb = foldr (\x (suma, cant) -> if x > umb then (suma+x, cant+1) else (suma, cant)) (0,0) xs
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral umb xs = 
  let (suma, cant) = foldr (\x (suma, cant) -> if x > umb then (suma+x, cant+1) else (suma, cant)) (0,0) xs
  in -- Si no hay números, evitamos división entre cero 
    if cant == 0 then Nothing 
    -- Usamos fromIntegral para tener Doubles y calcular prom
    else Just (suma / fromIntegral cant) 

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap f xs = foldr (\x lista -> f x : lista) [] xs

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined
