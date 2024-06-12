module Library where
import PdePreludat
import ShowFunction ()
import Text.Show.Functions ()
import GHC.OldList (sort)

--PUNTO 1----------------------------------------------
--Ciudad = nombre, año de fundacion, atracciones y costo de vida (Tupla/Data)
data Ciudad = Ciudad {
    nombre :: String,
    anioFundacion :: Number,
    atracciones :: [String], --lista va cambiando en cada caso de prueba
    costoDeVida :: Number
} deriving Show


valorDeCiudad :: Ciudad -> Number
valorDeCiudad ciudad
  | anioFundacion ciudad < 1800
                = (*5) (1800 - anioFundacion ciudad)
  | null (atracciones ciudad)  -- atracciones ciudad == []
                = (*2) (costoDeVida ciudad)
  | otherwise
                = (*3) (costoDeVida ciudad)

--PUNTO 2-----------------------------------------------------
-- "Alguna atraccion copada" Emi
isVowel :: Char -> Bool
isVowel character = character `elem` "aeiouAEIOU"


primeraLetraEsVocal :: [Char] -> Bool
primeraLetraEsVocal = isVowel.head

ciudadEsCopada :: Ciudad -> Bool
ciudadEsCopada ciudad= any primeraLetraEsVocal (atracciones ciudad)


--- Punto 2 Ciudad Sobria  LUCAS ------------------------------------
listaAtracciones :: Ciudad -> [String]
listaAtracciones (Ciudad _ _ atracciones _)= atracciones

cantidadLetras :: String -> Number
cantidadLetras ""= 0
cantidadLetras a = length a

esSobriaCiudad :: Number -> Ciudad  -> Bool
esSobriaCiudad cantMinLetras (Ciudad _ _ [] _) = True
esSobriaCiudad cantMinLetras (Ciudad _ _ atracciones _) = all ((>cantMinLetras) . cantidadLetras) atracciones

--"Ciudad con nombre raro" Fati

nombreRaro :: Ciudad -> Bool
nombreRaro  = (<5) . length . nombre

--PUNTO 3-----------------------------
--nueva atracción a la ciudad -> incremento del costo de vida de un 20%.

incrementoCostVida :: Ciudad -> Number -> Number      --Se define el calculo del porcentaje tal que valor+ valor*porcentaje de aumento (Reutilizable en caso de necesitar restar)
incrementoCostVida ciudad porcentaje_incremento = extraerCostVida ciudad + ((extraerCostVida ciudad * porcentaje_incremento)/ 100)

sumarNuevaAtraccion :: [String] -> Ciudad -> Ciudad
sumarNuevaAtraccion nueva ciudad  = ciudad {
    atracciones = atracciones ciudad ++ nueva,
    costoDeVida = incrementoCostVida ciudad 20
}

---Crisis Emi

eliminarUltimaAtraccion :: Ciudad -> Ciudad
eliminarUltimaAtraccion ciudad 
    | atracciones ciudad /= [] = ciudad {atracciones = init (atracciones ciudad), costoDeVida = incrementoCostVida ciudad (-10)}
    | otherwise = ciudad 

-- Remodelacion Lucas-----------------
extraerCostVida :: Ciudad->Number                     --Se definen funciones de extraccion de valores para mayor control
extraerCostVida ciudad = costoDeVida ciudad

extraerNombre :: Ciudad->String                        --Se definen funciones de extraccion de valores para mayor control
extraerNombre ciudad= (nombre ciudad)

nuevoNombre :: Ciudad-> String                         -- Se define una funcion de nuevo nombre para ser reutilizable
nuevoNombre ciudad= "New " ++ (extraerNombre ciudad)

incrementarCostoDeVida :: Number -> Ciudad -> Ciudad
incrementarCostoDeVida  porcentaje_incremento ciudad= ciudad {
    nombre = nuevoNombre ciudad,
    costoDeVida = incrementoCostVida ciudad porcentaje_incremento
}


--Reevaluacion Fati------

calculoAumentoDeVidaSiEsSobria :: Number -> Ciudad -> Number
calculoAumentoDeVidaSiEsSobria n ciudad
    |esSobriaCiudad n ciudad= incrementoCostVida ciudad 10
    |otherwise = costoDeVida ciudad -3


aumentoDeVidaSiEsSobria :: Number -> Ciudad -> Ciudad
aumentoDeVidaSiEsSobria n ciudad = ciudad {
    costoDeVida = calculoAumentoDeVidaSiEsSobria n ciudad
}


--Punto 4----------------

-- (aumentoDeVidaSiEsSobria  13 . eliminarUltimaAtraccion . incrementarCostoDeVida  10 . sumarNuevaAtraccion ["Buenos Aires"]) azul

--ENTREGA 2
------ Punto 5 ------------
data Anio = Anio {
    anioActual:: Number,
    eventos:: [Evento]
}

type Evento = Ciudad->Ciudad 
-- 5.1
aplicarEventosDeUnAnio   = foldl (\evento -> ($ evento) )  --El fold devuelve una funcion compuesta por todas las de la lista de eventos

pasoDeUnAnioEnCiudad ciudad anio = aplicarEventosDeUnAnio ciudad (eventos anio) 

-- 5.2
cantAtracciones :: Ciudad -> Number
cantAtracciones = length.atracciones

ciudadAumento :: Ord a => b -> (b -> a) -> (b -> b) -> Bool
ciudadAumento ciudad criterio evento  = (criterio . evento) ciudad > criterio ciudad

-- 5.3 

listaEventosMejoranCiudad :: Ord a => Ciudad -> Anio -> (Ciudad -> a) -> [Ciudad -> Ciudad]
listaEventosMejoranCiudad ciudad anio aComparar = filter (ciudadAumento ciudad aComparar) . eventos $ anio

costoDeVidaQueSuba :: Anio -> Ciudad -> Ciudad
costoDeVidaQueSuba anio ciudad = aplicarEventosDeUnAnio ciudad (listaEventosMejoranCiudad ciudad anio costoDeVida )

-- 5.4 

listaEventosCostoDeVidaBajo :: Ciudad -> Anio -> [Ciudad -> Ciudad]
listaEventosCostoDeVidaBajo ciudad anio = (filter (not.ciudadAumento ciudad costoDeVida) . eventos) anio

costoDeVidaQueBaje :: Anio -> Ciudad -> Ciudad
costoDeVidaQueBaje anio ciudad = aplicarEventosDeUnAnio ciudad (listaEventosCostoDeVidaBajo ciudad anio)

--5.5

queSubaValorDeCiudad :: Anio -> Ciudad -> Ciudad
queSubaValorDeCiudad anio ciudad = aplicarEventosDeUnAnio ciudad (listaEventosMejoranCiudad ciudad anio valorDeCiudad)

-- Punto 6

-- 6.1

eventosOrdenado :: Ciudad -> Anio -> Bool
eventosOrdenado ciudad anio = esOrdenada (map (costoDeVida) (map ($ ciudad) (eventos anio)))  

esOrdenada :: Ord a => [a] -> Bool
esOrdenada []  = True
esOrdenada [_]  = True
esOrdenada (x:y:xs) 
    | (<=) x y = esOrdenada (y:xs) 
    | otherwise = False

-- 6.2

ciudadesOrdenadas :: Evento -> [Ciudad] -> Bool
ciudadesOrdenadas evento ciudades = esOrdenada (map (costoDeVida . evento) ciudades) 

--6.3
aniosOrdenados :: [Anio] -> Ciudad -> Bool
aniosOrdenados listaAnios ciudad =  esOrdenada  (listaDeCiudades) 
    where listaDeCiudades = map ((costoDeVida . aplicarEventosDeUnAnio ciudad) . (eventos $)) listaAnios 
 
-- 7 Integrante 1
-- 7.1
{-
a2024 = Anio {
    anioActual = 2024,
    eventos = [eliminarUltimaAtraccion,aumentoDeVidaSiEsSobria 7, incrementarCostoDeVida 1, incrementarCostoDeVida 2, incrementarCostoDeVida 3, incrementarCostoDeVida 4,incrementarCostoDeVida 5] 
    }

RTA En este caso si puede haber un resultado posible que seria FALSE ya que la funcion "aumentoDeVidaSiEsSobria 7" > "incrementarCostoDeVida 1" por lo tanto la funcion del ej 6.1 solo evalua los valores que realmente se necesitan mejorando el rendimiento al evitar cálculos innecesarios.
--}

--7.2
{-
baradero :: Ciudad
baradero = Ciudad {
    nombre = "Baradero",
    anioFundacion = 1615,
    atracciones = ["Parque del Este", "Museo Alejandro Barbich" ],
    costoDeVida = 150
}
nullish :: Ciudad
nullish = Ciudad {
    nombre = "Nullish",
    anioFundacion = 1800,
    atracciones = [],
    costoDeVida = 140
}
caleta :: Ciudad
caleta = Ciudad {
    nombre = "Caleta Olivia",
    anioFundacion = 1901,
    atracciones = ["El Gorosito", "Faro Costanera"],
    costoDeVida = 120
}
azul :: Ciudad
azul = Ciudad {
    nombre = "Azul",
    anioFundacion = 1832,
    atracciones = ["Teatro Español", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"],
    costoDeVida = 190
}
listaDiscoRayado = [azul, nullish, caleta, baradero]

ultimosDosElementos:: [Ciudad] -> [Ciudad]
ultimosDosElementos  = (take 2).reverse

cicladoUltimosDos  = listaDiscoRayado ++ (cycle.ultimosDosElementos $ listaDiscoRayado)

RTA: Depende, en el caso de que se decida pasar x numeros de elementos, la funcion podria funcionar. En cambio 
en el caso de que se decida pasar la lista completa y no este ordenada realmente, por lazy evaluation habra un
momento en el cual la lista corte y de falso. En cambio, llegado el caso de que TODA LA  LISTA ESTE ORDENADA y 
se pase toda la lista, ahi si dependera de la capacidad computacional pero tarde o temprano se rompera por 
tiempo de procesamiento
-}



-- 7  integrante 3 Fati

--laHistoriaSinFIn = [anio2021, anio2022, anio2023, anio2023, anio2023, anio2023, anio2023, anio2023, anio2023, anio2023, anio2023, anio2023, anio2023, anio2023] --repeat anio2023

--Si va a ver un resultado, por lazy evaluation, en este caso va dar a dar falso porque para: 
--anio2021 el costoDeVida para baradero da 162
--anio2022 el costoDeVida para baradero da 155,925
--anio2023 el costoDeVida para baradero da 213,84
-- ...
--Da desordenado y por LAZY EVALUATION, que solo evalúa aquello que realmente se necesita para funcion, devuelve FALSE.

