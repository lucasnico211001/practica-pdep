module Library where
import PdePreludat
doble :: Number -> Number
doble numero = numero + numero
 

data Edificio = Edificio {
    pisos ::[Piso],
    valorm2 :: Number,
    robustez :: Number
}
 --Revisar la logica lista de lista de tupla
data Piso = Piso {
    numeroPiso :: Number,
    departamentos :: [Departamentos]
}
type Departamentos = (M2 , PorcentajeHab )
type M2 =Number 
type PorcentajeHab= Number 

m2:: Departamentos -> M2
m2 (y,_)=y

habitabilidad:: Departamentos -> PorcentajeHab
habitabilidad (_,z)=z

listaDptos:: Edificio -> [Number]
listaDptos edificio = (map  (length) (map departamentos (pisos edificio)))


edificioCheto:: Edificio -> Bool
edificioCheto edificio = all (==1) . listaDptos $ edificio

pajarera:: Edificio -> Bool 
pajarera edificio = all (>=6) . listaDptos $ edificio

--Aca no me toma sort, pero esta en la guia de lenguajes y lo he usado en los tps... 
piramide:: Edificio -> Bool 
piramide edificio =    ( sort (listaDepartamentos)) == listaDepartamentos 
    where listaDepartamentos= listaDptos $ edificio

listaDatosDptos :: Edificio -> [Departamentos]
listaDatosDptos edificio = concat (map (departamentos) (pisos edificio))

precioMaxDepartamentos :: Edificio -> Number
precioMaxDepartamentos edificio = maximum (map (*valor) (map m2 . listaDatosDptos $ edificio))
    where 
        valor= valorm2 edificio * robustez edificio 

merge:: [Departamentos] -> Departamentos
merge listaDptos = ( sum.map m2 $ listaDptos , sumhabit/cantdptos )
    where  
        cantdptos= length listaDptos
        sumhabit = sum. map habitabilidad $ listaDptos

split:: Number -> Departamentos -> [Departamentos]
split cantidad depto = replicate cantidad ( (/cantidad) . m2 $ depto, habitabilidad depto )

efectoDpto:: Number -> Departamentos -> Departamentos
efectoDpto reduccion depto 
    | (habitabilidad depto -reduccion)>0 = ( m2 depto, habitabilidad depto -reduccion)
    | otherwise= ( m2 depto, 0)

efectoPiso :: t -> Number -> (t -> Number -> Bool) -> Piso -> Piso
efectoPiso pisoAfectado reduccion condicion piso 
    | (condicion pisoAfectado).numeroPiso $ piso = Piso { numeroPiso=numeroPiso piso, departamentos= map (efectoDpto reduccion) (departamentos $ piso)}
    | otherwise =  piso

incendio:: Number -> Edificio -> Edificio
incendio pisoAfectado edificio = edificio {pisos= map (efectoPiso pisoAfectado 30 (>=) ) (pisos edificio), robustez= (/2).robustez $ edificio}

plaga :: Number -> Number -> Edificio -> Edificio
plaga pisoAfectado reduccion edificio= edificio {pisos = map (efectoPiso pisoAfectado reduccion (==) ) (pisos edificio)}

terremoto:: Number -> Edificio -> Edificio
terremoto valor edificio 
    | robustez edificio - valor >0 = edificio { robustez= robustez edificio - valor}
    | otherwise = edificio { robustez= 0}


ampliacion :: Number -> Number -> Edificio -> Edificio
ampliacion cantdptos metros edificio = edificio {pisos= pisos edificio ++ [nuevopiso]}
    where 
        nuevopiso= Piso {numeroPiso= 1 + maximum (map numeroPiso (pisos edificio)), departamentos= replicate cantdptos (metros/cantdptos, 100)}

restauracionDpto ::
  PorcentajeHab -> Number -> Departamentos -> (M2, Number)
restauracionDpto limite agregado departamento 
    | (habitabilidad departamento < limite) && (habitabilidad departamento + agregado<=100) = (m2 departamento, habitabilidad departamento + agregado)
    | (habitabilidad departamento >= limite ) = departamento
    | (habitabilidad departamento < limite) && (habitabilidad departamento + agregado>100) = (m2 departamento, 100)

restauracionPiso :: PorcentajeHab -> Number -> Piso -> Piso
restauracionPiso limite agregado piso =  Piso { numeroPiso=numeroPiso piso, departamentos= map (restauracionDpto limite agregado) (departamentos $ piso)}

fumigacion :: Edificio -> Edificio
fumigacion edificio = edificio {pisos=  map (restauracionPiso 60 20 ) . pisos $ edificio }

mergePiso :: Number -> Piso -> Piso
mergePiso nro piso 
    | numeroPiso piso == nro = Piso{numeroPiso= numeroPiso piso, departamentos= [merge.departamentos $ piso]}
    | otherwise = piso

mergeEdificio :: Number -> Edificio -> Edificio
mergeEdificio piso edificio = edificio { pisos= map (mergePiso piso)  (pisos edificio)}

splitPiso :: Number -> Number -> Piso -> Piso
splitPiso  cantidad nro  piso 
    | numeroPiso piso == nro = Piso {numeroPiso=numeroPiso piso, departamentos= reverse ((split cantidad (head.reverse.departamentos $ piso) ) )++ ( tail.reverse.departamentos $ piso) }
    | otherwise = piso 

splitEdificio :: Number -> Number -> Edificio -> Edificio
splitEdificio cantnuevosdptos numeroPiso edificio = edificio {pisos= map (splitPiso cantnuevosdptos numeroPiso) . pisos $ edificio }

funcionLoca ::Ord b1 => (a -> b1, [a] -> b2) -> (t -> b2 -> [a]) -> b1 -> [t] -> Bool
funcionLoca a b c = all ((>c) . fst a) . foldl (\x y -> b y . snd a $ x) []

--a es una tupla de dos elementos, debido a que usa de manera predeterminada las funciones fst y snd
-- el primer elemento de a es una funcion, esto se entiende como que la lista devuelta por el fold debe de hacer elemento a elemento ->
    -- la implementacion de la funcion que este en fst a, la cual retorna un valor del mismo tipo que c, y luego es comparado con C para devolver ->
    -- en caso de que todos cumplan la condicion de que al aplicarsele la funcion de a al elemento sea mayor que c
-- el segundo elemento de a es una funcion debido a que "\x y -> b y . snd a $ x" nos da a entender que esa funcion, compuesta con todo lo ->
    -- anterior se debe aplicar al elemento x
--b es una funcion porque debido a que "\x y -> b y . snd a $ x" nos muestra que es una funcion de dos parametros aplicada parcialmente en y ->
    -- que toma como segundo parametro el elemento x aplicada por la funcion ubicada en el segundo lugar de la tupla a
-- c es un elemento comparable
-- se visualiza tambien un elemento incognito que es una lista, debido a que al fold le falta el parametro lista pero tiene una lista vacia ->
    --como semilla 
-- devuelve un booleano debido a la validacion ALL que devuelve True o False segun que todos los elementos de la lista (segundo parametro) ->
    -- cumplan con la condicion pasada como primer parametro
