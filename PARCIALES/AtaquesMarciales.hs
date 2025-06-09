module AtaquesMarciales where
import Text.Show.Functions ()

data Peleador = UnPeleador{
    nombre :: String,
    vida :: Int,
    resistencia :: Int,
    ataques :: [Ataque]
} deriving Show

rocky :: Peleador
rocky = UnPeleador {
    nombre = "Rocky",
    vida = 200,
    resistencia = 100,
    ataques = [golpe 10,patada "nuca"]
} 

type Ataque = Peleador -> Peleador 

mapVida :: (Int -> Int) -> Peleador -> Peleador
mapVida unaModificacion unPeleador = unPeleador {vida = unaModificacion . vida $ unPeleador}

mapResistencia :: (Int -> Int) -> Peleador -> Peleador
mapResistencia unaModificacion unPeleador = unPeleador {resistencia = unaModificacion . resistencia $ unPeleador}

mapAtaque :: ([Ataque]->[Ataque])-> Peleador -> Peleador
mapAtaque unaModificacion unPeleador = unPeleador{ataques = unaModificacion . ataques $ unPeleador}

estaMuerto :: Peleador -> Bool
estaMuerto unPeleador = (== 0) . vida $ unPeleador 

esHabil :: Peleador -> Bool
esHabil unPeleador = cantidadDeAtaquesMayorA 10 unPeleador && resistenciaMayorA 15 unPeleador

cantidadDeAtaquesMayorA :: Int -> Peleador -> Bool
cantidadDeAtaquesMayorA unNumero unPeleador = (>unNumero) . length  . ataques $ unPeleador 

resistenciaMayorA :: Int -> Peleador -> Bool
resistenciaMayorA unNumero unPeleador = (>unNumero) . resistencia $ unPeleador 

perderVida :: Int -> Peleador -> Peleador
perderVida vidaAPerder unPeleador 
    | vida unPeleador < vidaAPerder = mapVida (const 0) unPeleador
    | otherwise = mapVida (subtract vidaAPerder) unPeleador

golpe :: Int -> Ataque
golpe unaIntensidad unPeleador =  mapVida (restarGolpe unaIntensidad (resistencia unPeleador)) unPeleador

restarGolpe :: Int -> Int -> Int -> Int 
restarGolpe unaIntensidad unaResistencia valor = subtract (div unaIntensidad unaResistencia) valor

toqueDeLaMuerte :: Ataque
toqueDeLaMuerte unPeleador = mapVida (const 0) unPeleador

type ParteDelCuerpo = String

es :: String -> ParteDelCuerpo -> Bool
es palabra parteDelCuerpo = (==palabra) parteDelCuerpo

patada :: ParteDelCuerpo -> Ataque 
patada parteDelCuerpo unPeleador 
    | es "pecho" parteDelCuerpo = darPatadaEnElPecho unPeleador
    | es "carita" parteDelCuerpo = darPatadaEnLaCara unPeleador
    | es "nuca" parteDelCuerpo = darPatadaEnLaNuca unPeleador
    | otherwise = unPeleador 

darPatadaEnElPecho :: Peleador -> Peleador
darPatadaEnElPecho unPeleador
    | not.estaMuerto $ unPeleador= (reducirResistencia . perderVida 10) unPeleador
    | otherwise = reducirResistencia . reanimar $ unPeleador

darPatadaEnLaCara :: Peleador -> Peleador 
darPatadaEnLaCara unPeleador = reducirResistencia . mapVida (subtract (div (vida unPeleador) 2)) $ unPeleador

darPatadaEnLaNuca :: Peleador -> Peleador
darPatadaEnLaNuca unPeleador = reducirResistencia . mapAtaque (drop 1) $ unPeleador 

reanimar :: Peleador -> Peleador
reanimar unPeleador = mapVida (+1) unPeleador

reducirResistencia :: Peleador -> Peleador
reducirResistencia unPeleador = mapResistencia (subtract 1) unPeleador

tripleAtaque :: Ataque -> Ataque -> Ataque -> Ataque 
tripleAtaque ataque1 ataque2 ataque3 unPeleador = ataque1 . ataque2 . ataque3 $ unPeleador

bruceLee :: Peleador
bruceLee = UnPeleador{
    nombre = "Bruce Lee",
    vida = 200,
    resistencia = 25,
    ataques = [patada "nuca",tripleAtaque (patada "carita") (patada "carita") (patada "Carita")]
} 

ataquesTerribles :: Peleador -> [Peleador] -> [Peleador]
ataquesTerribles unPeleador unosEnemigos = filter estaMuerto . map (aplicarAtaque unPeleador) $ unosEnemigos

aplicarAtaque :: Peleador -> Peleador -> Peleador
aplicarAtaque unPeleador unEnemigo = foldr ($) unEnemigo (ataques unPeleador) 

esPeligroso :: Peleador -> [Peleador] -> Bool
esPeligroso unPeleador unosEnemigos = any estaMuerto . map (aplicarAtaque unPeleador) $ unosEnemigos

