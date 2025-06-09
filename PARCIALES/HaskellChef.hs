module HaskellChef where
import Text.Show.Functions ()

data Participante = UnParticipante {
    nombre :: String,
    trucos :: [Truco],
    plato :: Plato
} deriving Show

data Plato = UnPlato {
    dificultad :: Int,
    componentes :: [(String, Int)]
} deriving Show

type Truco = Plato -> Plato
type Componente = (String, Int)
type TipoDePlato = Plato -> Bool

milanesa :: Plato
milanesa = UnPlato {
    dificultad = 20,
    componentes = [("pan rallado",30),("queso",30),("tomate",45)]
}

fideos :: Plato
fideos = UnPlato {
    dificultad = 30,
    componentes = [("harina",10),("agua",20),("queso",30),("tomate",45),("queso rallado",50),("manteca",5)]
}

pollo :: Plato
pollo = UnPlato {
    dificultad = 15,
    componentes = [("sal",3),("agua",20)]
}

mapDificultad :: (Int -> Int) -> Plato -> Plato
mapDificultad unaModificacion unPlato = unPlato {dificultad = unaModificacion . dificultad $ unPlato}

mapComponentes :: ([(String, Int)] -> [(String, Int)]) -> Plato -> Plato
mapComponentes unaModificacion unPlato = unPlato {componentes = unaModificacion . componentes $ unPlato}

aderezar :: String -> Int -> Truco
aderezar aderezo gramos unPlato = mapComponentes ((aderezo,gramos):) unPlato

endulzar :: Int -> Truco
endulzar gramos unPlato = aderezar "azucar" gramos unPlato

salar :: Int -> Truco
salar gramos unPlato = aderezar "sal" gramos unPlato

darSabor :: Int -> Int -> Truco
darSabor cantidadDeSal cantidadDeAzucar unPlato = (endulzar cantidadDeAzucar . salar cantidadDeSal) unPlato

duplicarPorcion :: Truco 
duplicarPorcion unPlato = mapComponentes (map duplicando) unPlato

duplicando :: (String, Int) -> (String, Int)
duplicando componentes = (fst componentes, snd componentes * 2)

simplificar :: Truco
simplificar unPlato
    | esUnBardo unPlato = simplificando unPlato
    | otherwise = unPlato

esUnBardo :: Plato -> Bool
esUnBardo unPlato = (esMayorA 5 . length . componentes $ unPlato)  && (esMayorA 7 . dificultad $ unPlato)

esMayorA :: Int -> Int -> Bool
esMayorA valor1 valor2 = (>valor1) valor2

simplificando :: Plato -> Plato
simplificando unPlato = (menosDificultad . sinComponentes) unPlato 

menosDificultad :: Plato -> Plato
menosDificultad unPlato = mapDificultad (const 5) unPlato 

sinComponentes :: Plato -> Plato
sinComponentes unPlato = mapComponentes (filter (masDeXGramos 10)) unPlato

masDeXGramos :: Int -> (String,Int) -> Bool
masDeXGramos valor componentes = (snd componentes) > valor

esVegano :: TipoDePlato
esVegano unPlato = not ((tieneAlimento "carne" unPlato) || (tieneAlimento "huevo" unPlato) || (tieneAlimento "lacteos" unPlato))

hayIngrediente :: String -> (String,Int) -> Bool
hayIngrediente ingrediente componentes = (==ingrediente) . fst $ componentes

tieneAlimento :: String -> Plato -> Bool
tieneAlimento ingrediente unPlato = any (hayIngrediente ingrediente) . componentes $ unPlato

esSinTacc :: TipoDePlato
esSinTacc unPlato = not . tieneAlimento "harina" $ unPlato

esComplejo :: TipoDePlato
esComplejo unPlato = esUnBardo unPlato

noAptoHipertension :: TipoDePlato
noAptoHipertension unPlato = any (masDeXGramos 2) . componentesConSal $ unPlato

componentesConSal :: Plato -> [(String,Int)]
componentesConSal unPlato = filter (hayIngrediente "sal") . componentes $ unPlato

pepeRonccino :: Participante
pepeRonccino = UnParticipante {
    nombre = "Pepe Ronccino",
    trucos = [darSabor 2 5, simplificar, duplicarPorcion],
    plato = milanesa
} 

cocinar :: [Truco] -> Plato -> Plato
cocinar trucos unPlato = foldr ($) unPlato trucos
--``
esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = tieneMasDificultad unPlato otroPlato && esMasLigero unPlato otroPlato

tieneMasDificultad :: Plato -> Plato -> Bool
tieneMasDificultad unPlato otroPlato = (>dificultad otroPlato) . dificultad $ unPlato

esMasLigero :: Plato -> Plato -> Bool
esMasLigero unPlato otroPlato = (<peso otroPlato) . peso $ unPlato

peso :: Plato -> Int
peso unPlato = sum . map snd . componentes $ unPlato

platinum :: Plato
platinum = UnPlato{
    dificultad = 10,
    componentes = componentesInfinitos
}

componentesInfinitos :: [Componente]
componentesInfinitos = map nuevosComponentes [1..]

nuevosComponentes :: Int -> Componente
nuevosComponentes numero = ("Ingrediente " ++ show numero, numero)
