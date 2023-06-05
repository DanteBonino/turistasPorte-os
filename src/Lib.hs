module Lib () where

--Cosas que te da el enunciado
juan =("Juan", playero)
ana = ("Ana", mejorCerca)
jorge = ("Jorge", gastronomico)
zulma = ("Zulma", playero)



---Punto 1:
type Lugar = (String, Int, [Atraccion])
type Persona = (String, (Lugar -> Bool))

data Atraccion = Atraccion{
    nombreAtraccion :: String,
    esComestible    :: Bool
}

--Si no se podría crear un criterio arbitrario que reciba un String -> Bool. Pero no sé bien a qué se refiere la consigna
--Punto 2:
lugaresALosQuePuedeIr :: Persona -> [Lugar] -> [String]
lugaresALosQuePuedeIr unaPersona  =  map nombreLugar . filter (snd unaPersona)

nombreLugar :: Lugar -> String
nombreLugar (unNombre,_, _) = unNombre


playero :: Lugar -> Bool
playero = tiene "Playa"

tiene :: String -> Lugar -> Bool
tiene unaAtraccion = elem unaAtraccion . map (nombreAtraccion) .  atracciones

atracciones :: Lugar -> [Atraccion]
atracciones  (_,_, atracciones) = atracciones

mejorCerca :: Lugar -> Bool
mejorCerca = (<500) . distancia

distancia :: Lugar -> Int
distancia (_,unaDistancia,_) = unaDistancia

gastronomico :: Lugar -> Bool
gastronomico = any (esComestible) . atracciones

--Punto 3:
lugarMasElegido :: [Persona] -> [Lugar] -> String
lugarMasElegido unasPersonas =  fst . mayorSegunCantidadDeTuristas . map (lugarSegunCantidadDeTuristasQueLoEligen unasPersonas) 

lugarSegunCantidadDeTuristasQueLoEligen :: [Persona] -> Lugar -> (String, Int)
lugarSegunCantidadDeTuristasQueLoEligen unasPersonas unLugar = (nombreLugar unLugar, cuantosPuedenIr unasPersonas unLugar)

cuantosPuedenIr ::  [Persona] -> Lugar -> Int
cuantosPuedenIr unasPersonas unLugar = foldr (sumarSiPuedeIr unLugar) 0 unasPersonas

sumarSiPuedeIr :: Lugar -> Persona -> ( Int -> Int)
sumarSiPuedeIr unLugar unaPersona
    | (snd unaPersona) unLugar = (+1)
    | otherwise                = (+0)
    
mayorSegunCantidadDeTuristas :: [(String, Int)] -> (String , Int)
mayorSegunCantidadDeTuristas  = foldl1 (mayorSegun snd)

mayorSegun :: (Ord b) => (a -> b) -> a -> a -> a
mayorSegun transf unValor otroValor
    | transf unValor > transf otroValor = unValor
    | otherwise                        = otroValor
