module PdePerritos where
import Text.Show.Functions ()

type Juguete = String
type Ejercicio = (Perrito -> Perrito)

data Perrito = UnPerrito {
    raza :: String,
    juguetesFavoritos :: [Juguete],
    tiempoEnGuarderia :: Int,
    energia :: Int
} deriving (Show, Eq)

data Guarderia = UnaGuarderia {
    nombre :: String,
    rutina :: [Actividad]
} deriving Show

data Actividad = UnaActividad {
    ejercicio :: Ejercicio,
    duracion :: Int
} deriving Show

peluche :: Perrito
peluche = UnPerrito {
    raza = "Caniche",
    juguetesFavoritos = ["Huesos", "Piedras", "Hojas"],
    tiempoEnGuarderia = 891,
    energia = 150
}

dalmata :: Perrito
dalmata = UnPerrito {
    raza = "dalmata",
    juguetesFavoritos = ["Coca Cola", "Tarea"],
    tiempoEnGuarderia = 892,
    energia = 200
}

zara :: Perrito
zara = UnPerrito {
    raza = "dalmata",
    juguetesFavoritos = ["Pelota", "Mantita"],
    tiempoEnGuarderia = 893,
    energia = 4
}

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = UnaGuarderia {
    nombre = "GuarderiaPdePerritos",
    rutina = [actividadJugar, actividadLadrar, actividadRegalarPelota, actividadDiaDeSpa, actividadDiaDeCampo] 
}

guarderiaSinSpa :: Guarderia
guarderiaSinSpa = UnaGuarderia {
    nombre = "Guarderia sin spa",
    rutina = [actividadJugar, actividadLadrar, actividadRegalarPelota, actividadDiaDeCampo]
}

jugar :: Ejercicio
jugar perrito = mapEnergia (subtract 10) perrito

actividadJugar :: Actividad
actividadJugar = UnaActividad {
    ejercicio = jugar,
    duracion = 30
}

ladrar :: Int -> Ejercicio
ladrar ladridos perrito = mapEnergia (+ (div ladridos 2)) perrito 

actividadLadrar :: Actividad
actividadLadrar = UnaActividad {
    ejercicio = ladrar 18,
    duracion = 20
}

mapEnergia :: (Int -> Int) -> Ejercicio
mapEnergia modificacion perrito = perrito {energia = max 0 . modificacion . energia $ perrito}

regalar :: Juguete -> Ejercicio
regalar juguete perrito = mapJuguetes (juguete:) perrito

actividadRegalarPelota :: Actividad
actividadRegalarPelota = UnaActividad {
    ejercicio = regalar "Pelota",
    duracion = 0
}

mapJuguetes :: ([Juguete] -> [Juguete]) -> Ejercicio 
mapJuguetes modificacion perrito = perrito {juguetesFavoritos = modificacion . juguetesFavoritos $ perrito}

diaDeSpa :: Ejercicio
diaDeSpa perrito
    | vaAPermanecer 50 perrito || esExtravagante perrito = mapEnergia (const 100) . regalar "peine" $ perrito
    | otherwise = perrito

vaAPermanecer :: Int -> Perrito -> Bool
vaAPermanecer tiempo perrito = (>= tiempo) . tiempoEnGuarderia $ perrito

esExtravagante :: Perrito -> Bool
esExtravagante (UnPerrito "dalmata" _ _ _) = True
esExtravagante (UnPerrito "pomerania" _ _ _) = True
esExtravagante _ = False 

actividadDiaDeSpa :: Actividad
actividadDiaDeSpa = UnaActividad {
    ejercicio = diaDeSpa,
    duracion = 120
}

diaDeCampo :: Ejercicio
diaDeCampo perrito = jugar . mapJuguetes (drop 1) $ perrito

actividadDiaDeCampo :: Actividad
actividadDiaDeCampo = UnaActividad {
    ejercicio = diaDeCampo,
    duracion = 720
}

puedeEstarEnGuarderia :: Perrito -> Guarderia -> Bool
puedeEstarEnGuarderia unPerrito unaGuarderia = (> tiempoRutina unaGuarderia).tiempoEnGuarderia $ unPerrito

tiempoRutina :: Guarderia -> Int
tiempoRutina unaGuarderia = sum . map duracion . rutina $ unaGuarderia

sonPerrosResponsables :: [Perrito] -> [Perrito]
sonPerrosResponsables unosPerritos = filter (tieneMasDeTresJuguetes . juguetesFavoritos . diaDeCampo) unosPerritos

tieneMasDeTresJuguetes :: [Juguete] -> Bool
tieneMasDeTresJuguetes listaDeJuguetes = (>3).length $ listaDeJuguetes

rutinaDeGuarderia :: Guarderia -> Perrito -> Perrito
rutinaDeGuarderia unaGuarderia unPerrito
    | tiempoRutina unaGuarderia <= tiempoEnGuarderia unPerrito = foldl aplicarActividad unPerrito (rutina unaGuarderia)
    | otherwise = unPerrito

aplicarActividad :: Perrito -> Actividad -> Perrito
aplicarActividad unPerrito unaActividad = (ejercicio unaActividad) unPerrito

perrosCansados :: [Perrito] -> Guarderia -> [Perrito]
perrosCansados unosPerritos unaGuarderia = filter estaCansado (map (rutinaDeGuarderia unaGuarderia) unosPerritos)

estaCansado :: Perrito -> Bool
estaCansado unPerrito = (<5) . energia $ unPerrito

verEnergias :: [Perrito] -> Guarderia -> [Int]
verEnergias perritos guarderia = map (energia . rutinaDeGuarderia guarderia) perritos

sogasInfinitas :: [String]
sogasInfinitas = map nombrarSoguita [1..]

nombrarSoguita :: Int -> String
nombrarSoguita numero = "Soguita " ++ show numero

pi :: Perrito
pi = UnPerrito {
    raza = "Labrador",
    juguetesFavoritos = sogasInfinitas,
    tiempoEnGuarderia = 314,
    energia = 159
}

