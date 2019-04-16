data Pirata = Pirata{
    nombre::String,
    botin::Botin
}

type Botin = [Tesoro]
type Tesoro = (String, Int)
type Saqueo = 

jackSparrow = Pirata {
    nombre = "Jack Sparrow",
    botin = [("Brujula", 10000), ("Frasco de Arena", 0)]
}

davidJones = Pirata {
    nombre = "David Jones",
    botin = [("Caja musical",1)]
}

anneBonny = Pirata {
    nombre = "Anne Bonny",
    botin = [("Doblones",100), ("Frasco de Arena", 1)]
}

unPirata :: Pirata
unPirata =  Pirata "pirata" [("dsada",2), ("dsdas",1500), ("dadsaf",50000)]

cantTesoros :: Pirata -> Int
cantTesoros = (length.botin)

esAfortunado :: Pirata -> Bool
esAfortunado = ((>10000).sum.(map (valorTesoro)).botin)

valorTesoro :: Tesoro -> Int
valorTesoro (x,y) = y 

--tienenMismoTesoroConDistintoValor :: Pirata -> Pirata -> Bool
--tienenMismoTesoroConDistintoValor unPirata otroPirata = find (mismoTesoro botin unPirata) botin otroPirata

--mismoTesoro :: Botin -> Botin -> Bool
--mismoTesoro unBotin otroBotin =

tesoroMasValioso :: Pirata -> Int
tesoroMasValioso = (maximum.(map (valorTesoro)).botin)

agregarTesoro :: Pirata -> Tesoro -> Pirata
agregarTesoro pirata tesoro = pirata {botin = botin pirata ++ [tesoro]}

perderTesorosValiosos :: Pirata -> Pirata
perderTesorosValiosos pirata = pirata {botin = ((filter tesoroNoValioso).botin) pirata} 

tesoroNoValioso :: Tesoro -> Bool
tesoroNoValioso (x,y) = y < 100

removerTesoro :: Pirata -> String -> Pirata
removerTesoro pirata nombre = pirata {botin = filter (tesoroNoEsNombre nombre) (botin pirata)}

tesoroNoEsNombre :: String -> Tesoro -> Bool
tesoroNoEsNombre nombre (x,y) = x /= nombre

saquear :: Pirata -> Saqueo -> Tesoro -> Pirata