module Monopoly where
import Text.Show.Functions ()

data Jugador = UnJugador {
    nombre :: String,
    cantidadDeDinero :: Int,
    tacticaDeJuego :: String,
    propiedadesCompradas :: [Propiedad],
    acciones :: [Accion]
} deriving Show

type Propiedad = (String, Int)
type Accion = Jugador -> Jugador

carolina :: Jugador
carolina = UnJugador {
    nombre = "Carolina",
    cantidadDeDinero = 500,
    tacticaDeJuego = "Accionista",
    propiedadesCompradas = [("hotel",500),("barco",250)],
    acciones = [pasarPorElBanco, pagarAAccionistas]
}

manuel :: Jugador
manuel = UnJugador {
    nombre = "Manuel",
    cantidadDeDinero = 500,
    tacticaDeJuego = "Oferente singular",
    propiedadesCompradas = [("pelota",200),("casa",148)],
    acciones = [pasarPorElBanco,enojarse]
}

mapNombre :: (String -> String) -> Jugador -> Jugador
mapNombre unaModificacion unJugador = unJugador {nombre = unaModificacion . nombre $ unJugador}

mapCantidadDeDinero :: (Int -> Int) -> Jugador -> Jugador
mapCantidadDeDinero unaModificacion unJugador = unJugador {cantidadDeDinero = unaModificacion . cantidadDeDinero $ unJugador}

mapTacticaDeJuego :: (String -> String) -> Jugador -> Jugador
mapTacticaDeJuego unaModificacion unJugador = unJugador {tacticaDeJuego = unaModificacion . tacticaDeJuego $ unJugador}

mapPropiedadesCompradas :: ([Propiedad] -> [Propiedad]) -> Jugador -> Jugador
mapPropiedadesCompradas unaModificacion unJugador = unJugador {propiedadesCompradas = unaModificacion . propiedadesCompradas $ unJugador}

mapAcciones :: ([Accion] -> [Accion]) -> Jugador -> Jugador
mapAcciones unaModificacion unJugador = unJugador {acciones = unaModificacion . acciones $ unJugador}

pasarPorElBanco :: Accion
pasarPorElBanco unJugador = mapCantidadDeDinero (+40) . mapTacticaDeJuego (const "Comprador compulsivo") $ unJugador

enojarse :: Accion
enojarse unJugador = mapCantidadDeDinero (+50) . mapAcciones (gritar:) $ unJugador

gritar :: Accion
gritar unJugador = mapNombre ("AHHHH " ++) unJugador

subastar :: Propiedad -> Accion
subastar unaPropiedad unJugador
    | tieneTacticaRequerida unJugador = disfrutaSubasta unaPropiedad unJugador
    | otherwise = unJugador

tieneTacticaRequerida :: Jugador -> Bool
tieneTacticaRequerida unJugador = elem (tacticaDeJuego unJugador) ["Oferente singular","Accionista"]

disfrutaSubasta :: Propiedad -> Jugador -> Jugador
disfrutaSubasta unaPropiedad unJugador = mapCantidadDeDinero (subtract (snd unaPropiedad)) . mapPropiedadesCompradas (unaPropiedad:) $ unJugador
 
cobrarAlquileres :: Accion
cobrarAlquileres unJugador = mapCantidadDeDinero (subtract (cobrandoAlquiler unJugador)) unJugador

cobrandoAlquiler :: Jugador -> Int
cobrandoAlquiler unJugador = sum . map precioPropiedad . propiedadesCompradas $ unJugador

precioPropiedad :: Propiedad -> Int
precioPropiedad (nombre,precio)
    | precio < 150 = 10
    | otherwise = 20

pagarAAccionistas :: Accion
pagarAAccionistas unJugador
    | tieneTactica "Accionista" unJugador = mapCantidadDeDinero (+200) unJugador
    | otherwise = mapCantidadDeDinero (subtract 100) unJugador

tieneTactica :: String -> Jugador -> Bool
tieneTactica unNombre unJugador = (==unNombre) . tacticaDeJuego $ unJugador
