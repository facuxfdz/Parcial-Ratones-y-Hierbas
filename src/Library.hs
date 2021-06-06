module Library where
import PdePreludat

-- PUNTO 1
type Enfermedad = String

data Raton = UnRaton{
    nombre :: String
,   edad :: Number 
,   peso :: Number 
,   enfermedades :: [Enfermedad]
}

cerebro :: Raton
cerebro = UnRaton "cerebro" 9 0.2 ["brucelosis","sarampion","tuberculosis"]

bicenterrata :: Raton
bicenterrata = UnRaton "bicenterrata" 256 0.2 []

huesudo :: Raton
huesudo = UnRaton "huesudo" 4 10 ["alta obesidad","sinusitis"]


-- PUNTO 2
type Hierba = Raton -> Raton
hierbaBuena :: Hierba
hierbaBuena = rejuvenecer

rejuvenecer :: Raton -> Raton
rejuvenecer raton = raton{edad = sqrt (edad raton) }

hierbaVerde :: String -> Hierba
hierbaVerde = eliminarEnfermedadesPor

hierbaZort :: Hierba
hierbaZort = transformarEnPinky

hierbaDelDiablo :: Hierba
hierbaDelDiablo = eliminarEnfermedadesMenosDe10 . perderPeso

alcachofa :: Hierba
alcachofa raton
    | peso raton > 2 = perderPesoEn 0.1 raton
    | otherwise  = perderPesoEn 0.05 raton

perderPesoEn :: Number -> Raton -> Raton
perderPesoEn porcentaje raton = raton{peso = modificarPesoEn (-porcentaje * peso raton) raton}

modificarPesoEn :: Number -> Raton -> Number
modificarPesoEn cantidad = (+cantidad) . peso

perderPeso :: Raton -> Raton
perderPeso raton = raton{ peso = modificarPesoEn (min 0 (-0.1)) raton}

eliminarEnfermedadesPor :: String -> Raton -> Raton
eliminarEnfermedadesPor terminacion raton = raton{enfermedades = filtrarEnfermedadesPor (not . terminaEn terminacion) raton}

eliminarEnfermedadesMenosDe10 :: Raton -> Raton
eliminarEnfermedadesMenosDe10 raton = raton{enfermedades=filtrarEnfermedadesPor ((>=10).length) raton}

filtrarEnfermedadesPor :: (String -> Bool) -> Raton -> [Enfermedad]
filtrarEnfermedadesPor condicion raton = filter condicion (enfermedades raton)

terminaEn :: String -> Enfermedad -> Bool 
terminaEn terminacion = (==terminacion) . reverse . take (length terminacion) . reverse

transformarEnPinky :: Raton -> Raton
transformarEnPinky raton = raton{enfermedades=[],edad=0}

-- PUNTO 3
type Medicamento = Raton -> Raton
type Potencia = Number 

pondsAntiAge :: Medicamento
pondsAntiAge = administrarHierbas [hierbaBuena,hierbaBuena,hierbaBuena,alcachofa]

reduceFatFast :: Potencia -> Medicamento
reduceFatFast potencia = administrarHierbas (replicate potencia alcachofa ++  [hierbaVerde "obesidad"])

pdepCilina :: Medicamento
pdepCilina = curarConHierbasVerdes

curarConHierbasVerdes :: Raton -> Raton
curarConHierbasVerdes raton = foldl (flip hierbaVerde) raton sufijosInfecciosas 

administrarHierbas :: [Hierba] -> Raton -> Raton
administrarHierbas hierbas raton = foldl (\raton hierba -> hierba raton) raton hierbas

sufijosInfecciosas :: [String]
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]