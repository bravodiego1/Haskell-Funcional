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

-- jugar :: Ejercicio
-- jugar perrito = perrito {energia = max 0 (energia perrito - 10)}  

-- jugar :: Ejercicio
-- jugar perrito = perrito {energia = max 0 . (subtract 10) . energia $ perrito} 

jugar :: Ejercicio
jugar perrito = mapEnergia (subtract 10) perrito

actividadJugar :: Actividad
actividadJugar = UnaActividad {
    ejercicio = jugar,
    duracion = 30
}

-- ladrar :: Int -> Ejercicio
-- ladrar ladridos perrito = perrito {energia = (+ (div ladridos 2)) . energia $ perrito}

ladrar :: Int -> Ejercicio
ladrar ladridos perrito = mapEnergia (+ (div ladridos 2)) perrito 

actividadLadrar :: Actividad
actividadLadrar = UnaActividad {
    ejercicio = ladrar 18,
    duracion = 20
}

-- ladrar :: Int -> Ejercicio
-- ladrar ladridos perrito = perrito {energia = (+ energia perrito) . flip div 2 $ ladridos}

mapEnergia :: (Int -> Int) -> Ejercicio
mapEnergia modificacion perrito = perrito {energia = max 0 . modificacion . energia $ perrito}

-- regalar :: Juguete -> Ejercicio
-- regalar juguete perrito = perrito {juguetesFavoritos = [juguetesFavoritos perrito ++ [juguete]}

-- regalar :: Juguete -> Ejercicio
-- regalar juguete perrito = mapJuguetes ((:) juguete) perrito

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

-- Solucion artesanal mia: 
-- esExtravagante :: Perrito -> Bool
-- esExtravagante perrito = ((== "dalmata").raza $ perrito) || ((== "pomerania").raza $ perrito)

-- SOLUCION ALTERNATIVA CON ELEM
-- esExtravaganteAlternativo :: Perrito -> Bool
-- esExtravaganteAlternativo perrito = elem (raza perrito) ["dalmata", "pomerania"] 

diaDeCampo :: Ejercicio
diaDeCampo perrito = jugar . mapJuguetes (drop 1) $ perrito

actividadDiaDeCampo :: Actividad
actividadDiaDeCampo = UnaActividad {
    ejercicio = diaDeCampo,
    duracion = 720
}

{- Tambien funcionaria esto: 
   diaDeCampo :: Ejercicio
   diaDeCampo perrito = mapJuguetes (drop 1) . jugar $ perrito -}

-- PARTE B

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

-- perrosCansados2 :: [Perrito] -> Guarderia -> [Perrito]
-- perrosCansados2 unosPerritos unaGuarderia = (filter estaCansado) . map (rutinaDeGuarderia unaGuarderia) $ unosPerritos

verEnergias :: [Perrito] -> Guarderia -> [Int]
verEnergias perritos guarderia = map (energia . rutinaDeGuarderia guarderia) perritos

-- Parte C

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

-- 1. ghci> esPerroExtravagante pi
-- Devuelve `False`.
-- Se puede porque evalúo solo la raza

-- 2.a ghci> tieneJuguete "Huesito" pi
-- *Depende*

-- No termina nunca porque tiene que trabajar con
-- la lista infinita entera para ver si alguno de
-- los elementos es "Huesito" y tiene infinitos jugutes *antes*.
-- En realidad depende de si le regalamos un huesito
-- antes de consultar porque la función solo puede terminar
-- cuando encuentra "Huesito" como elemento de la lista y al regalar
-- estoy poniendo al nuevo juguete como cabeza.

-- 2.b ghci> tieneJuguete "Pelota" . hacerRutinaDeGuarderia guarderia $ pi
-- Al hacer la rutina de la guardería se le regala una pelota
-- cuando recibe la pelota esta queda como primer elemento de la lista
-- y por eso la función puede terminar.

-- 2.c ghci>

-- ghci> tieneJuguete "Soguita 31112" pi
-- Devuelve `True`.

-- ghci> hacerRutinaDeGuarderia guarderia $ perroPi
-- La puede hacer (le cambia la energía y
-- otras cosas pero al mostrarlo por consola no va a
-- terminar de mostrar nunca).

-- ghci> regalar "Hueso" perroPi
-- Lo podemos hacer pero no se va a terminar de mostrar
-- al perro por consola.


