import Text.Show.Functions

--------------
-- Punto 01 --
--------------


data Auto = Auto{
    marca :: String, 
    modelo :: String,
    desgasteRuedas :: Float,
    desgasteChasis :: Float, 
    velocidadMaxima :: Float,
    tiempoCarrera :: Float
} deriving (Show)

-- Autos -- 
ferrariF50 :: Auto
ferrariF50 = "Ferrari" "F50" 0 0 65 0

lamborghiniDiablo :: Auto
lamborghiniDiablo = "Lamborghini" "Diablo" 4 7 73 0

fiat600 :: Auto
fiat600 = "Fiat" "600" 27 33 44 0

--------------
-- Punto 02 --
--------------

-- a --
estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado = desgasteChasis < 40 && desgasteRuedas < 60

-- b --
noDaMas :: Auto -> Bool
noDaMas = desgasteChasis > 80 || desgasteRuedas > 80

--------------
-- Punto 03 --
--------------

repararAuto :: Auto -> Auto
repararAuto unAuto = unAuto{
     desgasteChasis = desgasteChasis unAuto * 0.15 , desgasteRuedas = 0}