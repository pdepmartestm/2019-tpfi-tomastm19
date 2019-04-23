import Text.Show.Functions
main = print 0

data Pirata = Pirata{
    nombre :: String,
    botin :: Botin,
    saqueos :: [Saqueo]
} deriving (Show)

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
