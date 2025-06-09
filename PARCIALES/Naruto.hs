module Naruto where
import Text.Show.Functions () 

type Jutsu = Mision -> Mision
type Herramienta = (String,Int)

data Ninja = UnNinja {
    nombre :: String,
    herramientas :: [Herramienta],
    jutsus :: [Jutsu],
    rango :: Int
} deriving Show

naruto :: Ninja
naruto = UnNinja {
    nombre = "Naruto",
    herramientas = [("bomba de humo",2),("kunais",3)],
    jutsus = [clonesDeSombra 2,fuerzaDeUnCentenar],
    rango = 501
}

mapHerramientas :: ([Herramienta] -> [Herramienta]) -> Ninja -> Ninja
mapHerramientas unaModificacion unNinja = unNinja {herramientas = unaModificacion . herramientas $ unNinja}

mapRango :: (Int -> Int) -> Ninja -> Ninja
mapRango unaModificacion unNinja = unNinja {rango = max 0 . unaModificacion . rango $ unNinja}

obtenerHerramienta :: (String, Int) -> Ninja -> Ninja
obtenerHerramienta unaHerramienta unNinja
    | sumaDeHerramientasActuales unNinja + snd unaHerramienta <= 100 = mapHerramientas (unaHerramienta:) unNinja
    | otherwise = mapHerramientas ((fst unaHerramienta,herramientasQuePuedoSumar unNinja):) unNinja 

sumaDeHerramientasActuales :: Ninja -> Int
sumaDeHerramientasActuales unNinja = sum . map snd . herramientas $ unNinja

herramientasQuePuedoSumar :: Ninja ->  Int
herramientasQuePuedoSumar unNinja = 100 - (sumaDeHerramientasActuales unNinja)

usarHerramienta :: String -> Ninja -> Ninja
usarHerramienta herramienta unNinja = mapHerramientas (filter (nombreDistinto herramienta)) unNinja

nombreDistinto :: String -> Herramienta -> Bool
nombreDistinto unNombre unaHerramienta = (/=unNombre) . fst $ unaHerramienta 

data Mision = UnaMision {
    ninjasRequeridos :: Int,
    rangoRecomendable :: Int,
    enemigos :: [Ninja],
    recompensa :: Herramienta
} deriving Show

mapNinjasRequeridos :: (Int -> Int) -> Mision -> Mision
mapNinjasRequeridos unaModificacion unaMision = unaMision {ninjasRequeridos = max 1 . unaModificacion . ninjasRequeridos $ unaMision}

mapEnemigos :: ([Ninja] -> [Ninja]) -> Mision -> Mision
mapEnemigos unaModificacion unaMision = unaMision {enemigos = unaModificacion . enemigos $ unaMision}

esDesafiante :: [Ninja] -> Mision -> Bool
esDesafiante unosNinjas unaMision = (any (< rangoRecomendable unaMision) . map rango $ unosNinjas) && masDe2Enemigos unaMision

masDe2Enemigos :: Mision -> Bool
masDe2Enemigos unaMision = (>2) . length . enemigos $ unaMision

esCopada :: Mision -> Bool
esCopada unaMision = elem (recompensa unaMision) recompensasCopadas

recompensasCopadas :: [Herramienta]
recompensasCopadas = [("bomba de humo",3),("shurikens",5),("kunais",14)]

esFactible :: [Ninja] -> Mision -> Bool
esFactible unosNinjas unaMision = (noEsDesafiante unosNinjas unaMision && tienenLoNecesario unosNinjas unaMision) || sumaDeHerramientasDelEquipo unosNinjas > 500

noEsDesafiante :: [Ninja] -> Mision -> Bool
noEsDesafiante unosNinjas unaMision = not . esDesafiante unosNinjas $ unaMision

sumaDeHerramientasDelEquipo :: [Ninja] -> Int
sumaDeHerramientasDelEquipo unosNinjas = sum . map sumaDeHerramientasActuales $ unosNinjas 

tienenLoNecesario :: [Ninja] -> Mision -> Bool
tienenLoNecesario unosNinjas unaMision =  (>= ninjasRequeridos unaMision) . length $ unosNinjas

fallarMision :: [Ninja] -> Mision -> [Ninja]
fallarMision unosNinjas unaMision = map (mapRango (subtract 2)) . filter (rangoAceptable unaMision) $ unosNinjas

rangoAceptable :: Mision -> Ninja -> Bool
rangoAceptable unaMision unNinja = rango unNinja > rangoRecomendable unaMision

cumplirMision :: [Ninja] -> Mision -> [Ninja]
cumplirMision unosNinjas unaMision = map (mapRango (+1)) . obtienenRecompensa unaMision $ unosNinjas

obtienenRecompensa :: Mision -> [Ninja] -> [Ninja]
obtienenRecompensa unaMision unosNinjas = map (obtenerHerramienta (fst . recompensa $ unaMision, snd . recompensa $ unaMision)) unosNinjas

clonesDeSombra :: Int -> Jutsu
clonesDeSombra unosClones unaMision = mapNinjasRequeridos (subtract unosClones) unaMision

fuerzaDeUnCentenar :: Jutsu
fuerzaDeUnCentenar unaMision = mapEnemigos (filter (rangoMayorA 500)) unaMision

rangoMayorA :: Int -> Ninja -> Bool
rangoMayorA valor unNinja = rango unNinja > valor

ejecutarMision :: [Ninja] -> Mision -> [Ninja]
ejecutarMision unosNinjas unaMision = completarMision unosNinjas . usarJutsus unosNinjas $ unaMision 
     
usarJutsus :: [Ninja] -> Mision -> Mision
usarJutsus unosNinjas unaMision = foldr ($) unaMision (concat (map jutsus unosNinjas)) 

completarMision :: [Ninja] -> Mision -> [Ninja]
completarMision unosNinjas unaMision
  | esCopada unaMision || esFactible unosNinjas unaMision = cumplirMision unosNinjas unaMision
  | otherwise = fallarMision unosNinjas unaMision  

granGuerraNinja :: Mision
granGuerraNinja = UnaMision {
    ninjasRequeridos = 100000,
    rangoRecomendable = 100,
    enemigos = enemigosInfinitos,
    recompensa = ("abanico de Madara Uchiha",1)
}

enemigosInfinitos :: [Ninja]
enemigosInfinitos = map generarVillano [1..]

generarVillano :: Int -> Ninja
generarVillano unNumero = (UnNinja {nombre = "Zetsu " ++ show unNumero, herramientas = [], jutsus =[], rango = 600})

