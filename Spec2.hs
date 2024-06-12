module Spec where
import PdePreludat
import Library
import Test.Hspec

--Modelos casos de prueba
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

maipu :: Ciudad
maipu = Ciudad {
    nombre = "Maipú",
    anioFundacion = 1878,
    atracciones = ["Fortín Kakel"],
    costoDeVida = 115
}

azul :: Ciudad
azul = Ciudad {
    nombre = "Azul",
    anioFundacion = 1832,
    atracciones = ["Teatro Español", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"],
    costoDeVida = 190
}


anio2015 :: Anio
anio2015= Anio {
    anioActual=2015,
    eventos = []
}
anio2021 :: Anio
anio2021 = Anio {
    anioActual = 2021,
    eventos = [eliminarUltimaAtraccion, sumarNuevaAtraccion ["playa"]] 
}

anio2022 :: Anio
anio2022= Anio{
    anioActual = 2022,
    eventos = [eliminarUltimaAtraccion, incrementarCostoDeVida 5, aumentoDeVidaSiEsSobria 7 ]
} 

anio2023 :: Anio
anio2023 = Anio {
    anioActual = 2023,
    eventos = [eliminarUltimaAtraccion, sumarNuevaAtraccion ["parque"], incrementarCostoDeVida 10, incrementarCostoDeVida 20]
}

--para el punto 6 . 3
listaAnios :: [Anio]
listaAnios = [anio2021, anio2022, anio2023]

listaAnios2 :: [Anio]
listaAnios2 = [anio2022, anio2021, anio2023]

laHistoriaSinFIn = [anio2021,anio2022] ++ repeat anio2023



correrTests :: IO ()
correrTests = hspec $ do
  describe "Test punto 1" $ do
    it "El valor de la ciudad Baradero es 925" $ do
      valorDeCiudad baradero `shouldBe` 925
    it "El valor de la ciudad Nullish es 280" $ do
        valorDeCiudad nullish `shouldBe` 280
    it "El valor de la ciudad Caleta Olivia es 360" $ do
      valorDeCiudad caleta `shouldBe` 360

  describe "Test punto 2 - Es copada" $ do
    it "La ciudad Baradero no tiene alguna atracción copada, por lo tanto la ciudad no es copada" $ do
      ciudadEsCopada baradero `shouldBe` False
    it "La ciudad Nullish no tiene alguna atracción copada, por lo tanto la ciudad no es copada" $ do
      ciudadEsCopada nullish `shouldBe` False
    it "La ciudad Caleta Olivia tiene una atracción copada, por lo tanto la ciudad es copada" $ do
      ciudadEsCopada caleta `shouldBe` True
  describe "Ciudad sobria" $ do
    it "La ciudad Baradero es sobria porque tiene atracciones con más de 14 letras"   $ do
      esSobriaCiudad 14 baradero `shouldBe` True 
    it "La ciudad Baradero no es sobria porque no tiene atracciones con más de 15 letras"   $ do
      esSobriaCiudad 15 baradero  `shouldBe` False
    it "La ciudad Nullish es sobria porque no tiene atracciones" $ do
      esSobriaCiudad 5 nullish  `shouldBe` True
  describe "Ciudad con nombre raro" $ do
    it "La ciudad Maipú no tiene un nombre raro porque tiene exactamente 5 letras" $ do
      nombreRaro maipu `shouldBe` False
    it "La ciudad Azul tiene un nombre raro porque tiene menos de 5 letras" $ do
      nombreRaro azul `shouldBe` True 

  describe "Test Punto 3 - Sumar una nueva atraccion" $ do
    it "Se sumo una nueva atracción a la ciudad Azul: Balneario Municipal Alte. Guillermo Brown" $ do
      atracciones (sumarNuevaAtraccion ["Balneario Municipal Alte. Guillermo Brown"] azul) `shouldBe` ["Teatro Español", "Parque Municipal Sarmiento", "Costanera Cacique Catriel", "Balneario Municipal Alte. Guillermo Brown"]
  describe "Crisis" $ do
    it "Se elimino una atracción por una crisis a la ciudad Azul, ahora tiene solo dos atracciones" $ do
       atracciones (eliminarUltimaAtraccion azul) `shouldBe` ["Teatro Español", "Parque Municipal Sarmiento"]
  describe "Remodelacion" $ do
    it "Se incremento un 50% de costo de vida a la ciudad Azul, ahora tiene 285"  $ do
      nombre (incrementarCostoDeVida 50 azul) `shouldBe` "New Azul"
      costoDeVida (incrementarCostoDeVida 50 azul) `shouldBe` 285
  describe "Reevaluacion" $ do 
    it "La ciudad Azul no es sobria porque no tiene atracciones con mas de 14 letras entonces se le redujo -3 al costo de vida, siendo igual a 187" $ do
      costoDeVida(aumentoDeVidaSiEsSobria 14 azul)`shouldBe` 187
    it "La ciudad Azul si es sobria porque tiene atracciones con mas 13 letras entonces se aumento su costo de vida un 10%, siendo igual a 209" $ do
      costoDeVida(aumentoDeVidaSiEsSobria 13 azul) `shouldBe` 209


  describe "Test punto 5.1" $ do
    it "En la ciudad Azul en el año 2022 sucedieron los eventos (crisis, remodelación de 5% ,una reevaluación de 7)" $ do
      nombre (pasoDeUnAnioEnCiudad azul anio2022) `shouldBe` "New Azul"
      costoDeVida (pasoDeUnAnioEnCiudad azul anio2022) `shouldBe` 197.505
    it "En la ciudad Azul en el año 2015 no sucedieron eventos" $ do
      costoDeVida (pasoDeUnAnioEnCiudad azul anio2015) `shouldBe` 190
 
  describe "Test punto 5.2" $ do
    it "Para la ciudad Azul con el evento crisis y criterio para comparar el costoDeVida." $ do
      ciudadAumento azul costoDeVida eliminarUltimaAtraccion  `shouldBe` False
    it "Para la ciudad Azul, el evento Agregar atracción (Monasterio Trapense) y el criterio para comparar el costo de vida" $ do
      ciudadAumento azul costoDeVida (sumarNuevaAtraccion ["Monasterio Trapense"]) `shouldBe` True
    it "Para la ciudad Azul, el evento Agregar atracción (Monasterio Trapense) y el criterio para comparar cantidad de atracciones" $ do
      ciudadAumento azul cantAtracciones (sumarNuevaAtraccion ["Monasterio Trapense"])  `shouldBe` True

  describe "Test punto 5.3" $ do
    it "Se actualizo la ciudad Azul: 'New Azul' con costo de vida = 219.45" $ do
      nombre (costoDeVidaQueSuba anio2022 azul) `shouldBe` "New Azul"
      costoDeVida (costoDeVidaQueSuba anio2022 azul) `shouldBe` 219.45

  describe "Test punto 5.4" $ do
    it "Se actualizo la ciudad Azul: 'New Azul' con costo de vida = 171 y con dos atracciones" $ do
      nombre (costoDeVidaQueBaje anio2022 azul) `shouldBe` "Azul"
      costoDeVida (costoDeVidaQueBaje anio2022 azul) `shouldBe` 171
      atracciones (costoDeVidaQueBaje anio2022 azul) `shouldBe` ["Teatro Español", "Parque Municipal Sarmiento"]

  describe "Test punto 5.5" $ do   
    it "Se actualizo la ciudad Nullish: 'New Nullish' con costo de vida 161.7" $ do
      nombre (queSubaValorDeCiudad anio2022 nullish) `shouldBe` "New Nullish"
      costoDeVida (queSubaValorDeCiudad anio2022 nullish) `shouldBe` 161.7

  describe "Test punto 6.1" $ do
    it "En el año 2022 sobre la ciudad Azul tiene los eventos ordenados" $ do
      eventosOrdenado azul anio2022 `shouldBe` True
    it "En el año 2023 sobre la ciudad Azul los eventos NO estan ordenados" $ do
      eventosOrdenado azul anio2023 `shouldBe` False

  
  describe "Test punto 6.2" $ do
    it "En el año 2022 sobre la ciudad Azul tiene los eventos ordenados" $ do
      ciudadesOrdenadas  (incrementarCostoDeVida 10) [caleta, nullish, baradero, azul] `shouldBe` True
    it "En el año 2023 sobre la ciudad Azul los eventos NO estan ordenados" $ do
      ciudadesOrdenadas (incrementarCostoDeVida 10)  [caleta, azul, baradero] `shouldBe` False

  describe "Test punto 6.3" $ do   
    it "Los eventos del año 2021, 2022, 2023 devuelven un costo de vida desordenado para Baradero" $ do
      aniosOrdenados listaAnios baradero `shouldBe` False
    it "Los eventos del año 2022, 2021, 2023 devuelven un costo de vida ordenado para Baradero" $ do
      aniosOrdenados listaAnios2 baradero `shouldBe` True

  describe "Test punto 7" $ do   
    it "LaHistoriaSinFin no esta ordenada" $ do
      aniosOrdenados laHistoriaSinFIn baradero `shouldBe` False