module PadrinosMagicos where
import Text.Show.Functions ()

type Deseo = Chico -> Chico
type Habilidad = String
type Padrino = Chico -> Chico
type Condicion = Chico -> Bool

data Chico = UnChico {
    nombre :: String,
    edad :: Int,
    habilidades :: [Habilidad],
    deseos :: [Deseo]
} deriving Show

data Chica = UnaChica {
    nombreChica :: String,
    condicion :: Condicion
} deriving Show

timmy :: Chico
timmy = UnChico {
    nombre = "Timmy",
    edad = 18,
    habilidades = ["comer muffins","jugar con chester"],
    deseos = [aprenderHabilidades ["matar"],serMayor]
}

trixie :: Chica
trixie = UnaChica {
    nombreChica = "Trixie",
    condicion = (tieneHabilidad "cocinar")
}

mapHabilidades :: ([Habilidad] -> [Habilidad]) -> Chico -> Chico
mapHabilidades unaModificacion unChico = unChico {habilidades = unaModificacion . habilidades $ unChico}

mapEdad :: (Int -> Int) -> Chico -> Chico
mapEdad unaModificacion unChico = unChico {edad = unaModificacion . edad $ unChico}

mapDeseos :: ([Deseo] -> [Deseo]) -> Chico -> Chico
mapDeseos unaModificacion unChico = unChico {deseos = unaModificacion . deseos $ unChico}

aprenderHabilidades :: [Habilidad] -> Deseo
aprenderHabilidades unasHabilidades unChico = mapHabilidades (++unasHabilidades) unChico

serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed unChico = mapHabilidades (++habilidadesInfinitas) unChico

habilidadesInfinitas :: [Habilidad]
habilidadesInfinitas = map nuevasHabilidades [1..]

nuevasHabilidades :: Int -> Habilidad
nuevasHabilidades unNumero = "jugar need for speed " ++ show unNumero

serMayor :: Deseo
serMayor unChico = mapEdad (const 18) unChico

wanda :: Padrino
wanda unChico = (cumplirPrimerDeseo . mapEdad (+1)) unChico 

cumplirPrimerDeseo :: Chico -> Chico
cumplirPrimerDeseo unChico = (head . deseos $ unChico) unChico

cosmo :: Padrino 
cosmo unChico = mapEdad desMadurar unChico

desMadurar :: Int -> Int
desMadurar unaEdad = div unaEdad 2

muffinMagico :: Chico -> Chico
muffinMagico unChico = foldr ($) unChico (deseos unChico)

tieneHabilidad :: Habilidad -> Condicion
tieneHabilidad unaHabilidad unChico = elem unaHabilidad . habilidades $ unChico

esSuperMaduro :: Condicion
esSuperMaduro unChico = esMayorDeEdad unChico && sabeManejar unChico

esMayorDeEdad :: Chico -> Bool
esMayorDeEdad unChico = (>= 18) . edad $ unChico

sabeManejar :: Chico -> Bool
sabeManejar unChico = tieneHabilidad "manejar" unChico

quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA unaChica unosPretendientes
    | any (cumpleCondicion unaChica) unosPretendientes = head (filter (cumpleCondicion unaChica) unosPretendientes)
    | otherwise = last unosPretendientes

cumpleCondicion :: Chica -> Chico -> Bool
cumpleCondicion unaChica unChico = (condicion unaChica) unChico

infractoresDaRules :: [Chico] -> [String]
infractoresDaRules listaDeChicos = map darNombre (filter tieneDeseoProhibido listaDeChicos)

darNombre :: Chico -> String
darNombre unChico = nombre unChico

tieneDeseoProhibido :: Chico -> Bool
tieneDeseoProhibido unChico = any esHabilidadProhibida . take 5 . habilidades . cumplirDeseos $ unChico

esHabilidadProhibida :: Habilidad -> Bool
esHabilidadProhibida habilidad = elem habilidad ["enamorar", "matar", "dominar el mundo"]

cumplirDeseos :: Chico -> Chico
cumplirDeseos unChico = foldr ($) unChico (deseos unChico)  












