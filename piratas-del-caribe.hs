import Text.Show.Functions
main = print 0

data Pirata = Pirata {
    nombre :: String,
    botin :: Botin,
    saqueos :: [Saqueo]
} deriving (Show)

data Barco = Barco {
    nombreBarco :: String,
    tripulacion :: [Pirata],
    saqueo :: Saqueo
} deriving (Show)

data Isla = Isla {
    nombreIsla :: String,
    elementoTipico :: Tesoro
}

data Ciudad = Ciudad {
    nombreCiudad :: String,
    tesoros :: Botin
}

type Botin = [Tesoro]
type Tesoro = (String, Int)
type Saqueo = Tesoro -> Bool

jackSparrow = Pirata {
    nombre = "Jack Sparrow",
    botin = [("Brujula", 10000), ("Frasco de Arena", 0)],
    saqueos = [saquearTesoroValioso, (saquearTesoroEspecifico "sombrero")]
}

davidJones = Pirata {
    nombre = "David Jones",
    botin = [("Caja musical",1)],
    saqueos = [noSaquear]
}

anneBonny = Pirata {
    nombre = "Anne Bonny",
    botin = [("Doblones",100), ("Frasco de Arena", 1)],
    saqueos = [saquearTesoroEspecifico "oro"]
}

perlaNegra = Barco {
    nombreBarco = "Perla Negra",
    tripulacion = [jackSparrow, anneBonny],
    saqueo = saquearTesoroValioso
}

holandesErrante = Barco {
    nombreBarco = "Holandes Errante",
    tripulacion = [davidJones],
    saqueo = noSaquear
}


-- *** Tesoros piratas *** 


cantTesoros :: Pirata -> Int
cantTesoros = (length.botin)

esAfortunado :: Pirata -> Bool
esAfortunado = ((>10000).sum.(map (valorTesoro)).botin)

valorTesoro :: Tesoro -> Int
valorTesoro (_, valor) = valor

tienenMismoTesoroConDistintoValor :: Pirata -> Pirata -> Bool
tienenMismoTesoroConDistintoValor unPirata otroPirata = any (tieneTesoroConDistintoValor (botin otroPirata)) (botin unPirata)

tieneTesoroConDistintoValor :: Botin -> Tesoro -> Bool
tieneTesoroConDistintoValor unBotin tesoro = any (mismoTesoroDistintoValor tesoro) unBotin 

mismoTesoroDistintoValor :: Tesoro -> Tesoro -> Bool
mismoTesoroDistintoValor (nombre1, valor1) (nombre2, valor2) = nombre1 == nombre2 &&  valor1 /=valor2

tesoroMasValioso :: Pirata -> Int
tesoroMasValioso = (maximum.(map (valorTesoro)).botin)

agregarTesoro :: Pirata -> Tesoro -> Pirata
agregarTesoro pirata tesoro = pirata {botin = botin pirata ++ [tesoro]}

perderTesorosValiosos :: Pirata -> Pirata
perderTesorosValiosos pirata = pirata {botin = ((filter tesoroNoValioso).botin) pirata} 

tesoroNoValioso :: Tesoro -> Bool
tesoroNoValioso (_, valor) = valor < 100

removerTesoro :: Pirata -> String -> Pirata
removerTesoro pirata nombre = pirata {botin = filter (tesoroNoEsNombre nombre) (botin pirata)}

tesoroNoEsNombre :: String -> Tesoro -> Bool
tesoroNoEsNombre nombre1 (nombre2, _) = nombre2 /= nombre1


-- *** Temporada de saqueos ***


saquearTesoroValioso :: Saqueo
saquearTesoroValioso = not.tesoroNoValioso

saquearTesoroEspecifico :: String -> Saqueo
saquearTesoroEspecifico nombre tesoro = fst tesoro == nombre

noSaquear :: Saqueo
noSaquear tesoro = False 

saquear :: Pirata -> Tesoro -> Pirata
saquear pirata tesoro | any (cumpleCondicion tesoro) (saqueos pirata) = agregarTesoro pirata tesoro
                      | otherwise = pirata

cumpleCondicion :: Tesoro -> Saqueo -> Bool
cumpleCondicion tesoro saqueo = saqueo tesoro


-- *** Navegando los siete mares ***  


agregarTripulante :: Barco -> Pirata -> Barco
agregarTripulante barco pirata = barco{tripulacion = tripulacion barco ++ [pirata]}

removerTripulante :: Barco -> Pirata -> Barco
removerTripulante barco pirata = barco{tripulacion = filter (tripulanteNoEsPirata pirata) (tripulacion barco)}

tripulanteNoEsPirata :: Pirata -> Pirata -> Bool
tripulanteNoEsPirata pirata1 pirata2 = (nombre pirata1) /= (nombre pirata2)

anclarEnIsla :: Barco -> Isla -> Barco
anclarEnIsla barco isla = barco {tripulacion = map ((flip agregarTesoro) (elementoTipico isla)) (tripulacion barco)}

atacarCiudad :: Barco -> Ciudad -> Barco
atacarCiudad barco ciudad =  barco {tripulacion = zipWith saquear (tripulacion barco) (tesoros ciudad)}

-- El barco con más tripulantes se lleva a los tripulantes del otro
abordarBarco :: Barco -> Barco -> Barco
abordarBarco barco1 barco2 | length (tripulacion barco1) > length (tripulacion barco2) = barco1 {tripulacion = tripulacion barco1 ++ tripulacion barco2}
                           | otherwise = barco2 {tripulacion = tripulacion barco2 ++ tripulacion barco1}