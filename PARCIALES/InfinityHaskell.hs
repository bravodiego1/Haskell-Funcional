module InfinityHaskell where
import Text.Show.Functions ()

data Personaje = UnPersonaje {
    nombre :: String,
    cantidadDePoder :: Int,
    derrotas :: [Derrota],
    equipamientos :: [Equipamiento]
} deriving Show

type Derrota = (String, Int)

ironMan :: Personaje
ironMan = UnPersonaje {
    nombre = "Iron Man",
    cantidadDePoder = 1500,
    derrotas = [hijoDeThanos],
    equipamientos = [trajeMecanizado 2]
}

capitanAmerica :: Personaje
capitanAmerica = UnPersonaje {
    nombre = "Capitan America",
    cantidadDePoder = 1200,
    derrotas = [hijoDeThanos],
    equipamientos = [escudo]
}

bucky :: Personaje
bucky = UnPersonaje {
    nombre = "El Soldado del Invierno",
    cantidadDePoder = 1000,
    derrotas = [],
    equipamientos = []
}

spiderman :: Personaje
spiderman = UnPersonaje {
    nombre = "Spiderman",
    cantidadDePoder = 2000,
    derrotas = [hijoDeThanos],
    equipamientos = []
}

thor :: Personaje
thor = UnPersonaje {
    nombre = "Thor",
    cantidadDePoder = 3000,
    derrotas = [hijoDeThanos],
    equipamientos = [stormBreaker]
}

thanos :: Personaje
thanos = UnPersonaje {
    nombre = "Thanos",
    cantidadDePoder = 5000,
    derrotas = [],
    equipamientos = [gemaDelAlma]
}

hijoDeThanos :: Derrota
hijoDeThanos = ("Hijo de Thanos", 2018)

entrenamiento :: [Personaje] -> [Personaje]
entrenamiento listaDePersonajes = map (entrenar (length listaDePersonajes)) listaDePersonajes

entrenar :: Int -> Personaje -> Personaje
entrenar multiplicadorDePoder unPersonaje = mapPoder (* multiplicadorDePoder) unPersonaje

mapPoder :: (Int -> Int) -> Personaje -> Personaje
mapPoder unaModificacion unPersonaje = unPersonaje {cantidadDePoder = unaModificacion . cantidadDePoder $ unPersonaje}

rivalesDignos :: [Personaje] -> [Personaje]
rivalesDignos listaDePersonajes =  filter esDigno . entrenamiento $ listaDePersonajes

poderMayorA :: Int -> Personaje -> Bool
poderMayorA valor unPersonaje = (>valor).cantidadDePoder $ unPersonaje

perdioConThanos :: Personaje -> Bool
perdioConThanos unPersonaje =  elem "Hijo de Thanos" . oponentes $ unPersonaje

oponentes :: Personaje -> [String]
oponentes unPersonaje = map fst . derrotas $ unPersonaje

esDigno :: Personaje -> Bool
esDigno unPersonaje = poderMayorA 500 unPersonaje && perdioConThanos unPersonaje 

guerraCivil :: Int -> [Personaje] -> [Personaje] -> [Personaje]
guerraCivil anio listaDePersonajesA listaDePersonajesB = zipWith (peleaDePersonajes anio) listaDePersonajesA listaDePersonajesB

peleaDePersonajes :: Int -> Personaje -> Personaje -> Personaje
peleaDePersonajes anio personajeA personajeB
  | poderMayorA (cantidadDePoder personajeB) personajeA = mapDerrota ((nombre personajeB, anio):) personajeA
  | otherwise = mapDerrota ((nombre personajeA, anio):) personajeB

mapDerrota :: ([Derrota] -> [Derrota]) -> Personaje -> Personaje
mapDerrota modificacion unPersonaje = unPersonaje {derrotas = modificacion . derrotas $ unPersonaje} 

type Equipamiento = Personaje -> Personaje

escudo :: Equipamiento
escudo unPersonaje
    |  (<5) . length . derrotas $ unPersonaje = mapPoder (+50) unPersonaje
    | otherwise = mapPoder (subtract 100) unPersonaje

trajeMecanizado :: Int -> Equipamiento
trajeMecanizado versionDelTraje unPersonaje = mapNombre (agregaIron . agregaVersion versionDelTraje) unPersonaje

agregaIron :: String -> String
agregaIron palabra = "Iron " ++ palabra

agregaVersion :: Int -> String -> String
agregaVersion numero palabra = palabra ++ ", V" ++ show numero

mapNombre :: (String -> String) -> Personaje -> Personaje
mapNombre unaModificacion unPersonaje = unPersonaje {nombre = unaModificacion . nombre $ unPersonaje}

equipamientoExclusivo :: String -> Personaje -> Equipamiento -> Personaje 
equipamientoExclusivo unNombre unPersonaje unEquipamiento 
    | (==unNombre) . nombre $ unPersonaje = unEquipamiento unPersonaje
    | otherwise = unPersonaje

stormBreaker :: Equipamiento
stormBreaker unPersonaje = equipamientoExclusivo "Thor" unPersonaje (diosBondadoso . agregarDiosDelTrueno) 

diosBondadoso :: Personaje -> Personaje 
diosBondadoso unPersonaje =  mapDerrota (const []) unPersonaje

agregarDiosDelTrueno :: Personaje -> Personaje
agregarDiosDelTrueno unPersonaje = mapNombre (++ " dios del trueno") unPersonaje

gemaDelAlma :: Equipamiento
gemaDelAlma unPersonaje = equipamientoExclusivo "Thanos" unPersonaje agregarDerrotasInfinitas

agregarDerrotasInfinitas :: Personaje -> Personaje
agregarDerrotasInfinitas unPersonaje = mapDerrota (++derrotasInfinitas) unPersonaje

derrotasInfinitas :: [Derrota]
derrotasInfinitas = zip extrasInfinitos [2025..]

extrasInfinitos :: [String]
extrasInfinitos = map (("Extra numero " ++) . show) [1..]

guanteleteInfinito :: Equipamiento
guanteleteInfinito unPersonaje = equipamientoExclusivo "Thanos" unPersonaje usarGemasDelInfinito

usarGemasDelInfinito :: [Equipamiento] -> Personaje -> Personaje
usarGemasDelInfinito equipamientos unPersonaje = foldr ($) unPersonaje (filter esGemaDelInfinito equipamientos)