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

eliminarEnfermedades :: String -> Raton -> Raton
eliminarEnfermedades terminacion raton = raton{enfermedades=filter (not . terminaEn terminacion) (enfermedades raton)}

terminaEn :: String -> Enfermedad -> Bool 
terminaEn terminacion enfermedad = take (length terminacion) (reverse enfermedad) == terminacion 
-- PUNTO 3
-- type Medicamento = [Hierba]