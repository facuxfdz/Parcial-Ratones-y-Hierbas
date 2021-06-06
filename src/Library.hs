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
hierbaVerde = eliminarEnfermedades

alcachofa :: Raton -> Raton
alcachofa raton
    | peso raton > 2 = perderPesoEn 0.1 raton
    | otherwise  = perderPesoEn 0.05 raton

perderPesoEn :: Number -> Raton -> Raton
perderPesoEn porcentaje raton = raton{peso = modificarPesoEn (-porcentaje * peso raton) raton}

modificarPesoEn :: Number -> Raton -> Number
modificarPesoEn cantidad = (+cantidad) . peso

eliminarEnfermedades :: String -> Raton -> Raton
eliminarEnfermedades terminacion raton = raton{enfermedades = filtrarEnfermedadesPor (not . terminaEn terminacion) raton}

filtrarEnfermedadesPor :: (String -> Bool) -> Raton -> [Enfermedad]
filtrarEnfermedadesPor condicion raton = filter condicion (enfermedades raton)

terminaEn :: String -> Enfermedad -> Bool 
terminaEn terminacion = (==terminacion) . take (length terminacion) . reverse

-- PUNTO 3
-- type Medicamento = [Hierba]