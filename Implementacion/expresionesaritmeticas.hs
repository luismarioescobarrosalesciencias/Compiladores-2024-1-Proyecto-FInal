{-# LANGUAGE InstanceSigs #-}


--Lenguaje de Expresiones aritméticas  
data ExpAritmetica
  = NumAritmetica Int
  | VarAritmetica String 
  | AsignacionAritmetica String ExpAritmetica
  | SumaAritmetica ExpAritmetica ExpAritmetica
  | MultAritmetica ExpAritmetica ExpAritmetica
  | ResAritmetica ExpAritmetica ExpAritmetica


-- Contexto de variables 
type Contexto = [(String, Int)]

-- Tipo de excepciones
data Excepcion = VariableNoEncontradaAritmetica | NoEsExpAritmeticaAritmetica
               | NoEntradaNatAritmetica | NoCalculoNatAritmetica

-- Nueva estructura monádica para manejar excepciones en el nuevo lenguaje
data EvalExpAritmetica a = Fallo Excepcion | ExitoAritmetica a

instance Show (Excepcion) where
    show VariableNoEncontradaAritmetica = "Variable aritmetica no encontrada"
    show NoEsExpAritmeticaAritmetica = "No es expresion Aritmetica"
    show NoEntradaNatAritmetica = "El dato no es un numero natural"
    show NoCalculoNatAritmetica = "El resultado no es un numero natural "

instance Show a => Show (EvalExpAritmetica a) where
  show (Fallo e) = "Fallo " ++ show e
  show (ExitoAritmetica a) = "ExitoAritmetica " ++ show a

-- Nuevas instancias necesarias para convertir EvalExpAritmetica en un Functor, Applicative, y Monad
instance Functor EvalExpAritmetica where
  fmap _ (Fallo e) = Fallo e
  fmap f (ExitoAritmetica s) = ExitoAritmetica (f s)

instance Applicative EvalExpAritmetica where
  pure elem = ExitoAritmetica elem

  (<*>) :: EvalExpAritmetica (a -> b) -> EvalExpAritmetica a -> EvalExpAritmetica b
  (Fallo e) <*> _ = Fallo e
  _ <*> (Fallo e) = Fallo e
  (ExitoAritmetica m) <*> a = fmap m a



instance Monad EvalExpAritmetica where
  Fallo e >>= _ = Fallo e
  ExitoAritmetica a >>= m = m a

---
-- Evaluación
---

-- Evaluación de expresiones aritméticas para el nuevo lenguaje
evalExpAritmetica :: ExpAritmetica -> Contexto -> EvalExpAritmetica Int
evalExpAritmetica (NumAritmetica n) _ 
  | n < 0 = Fallo NoEntradaNatAritmetica
  | otherwise = ExitoAritmetica n

evalExpAritmetica (VarAritmetica c) contexto = buscar (VarAritmetica c) contexto contexto

evalExpAritmetica (AsignacionAritmetica id exp) contexto = do
  valorExp <- evalExpAritmetica exp contexto
  ExitoAritmetica valorExp

evalExpAritmetica (SumaAritmetica exp1 exp2) contexto = do
  valorExp1 <- evalExpAritmetica exp1 contexto
  valorExp2 <- evalExpAritmetica exp2 contexto
  ExitoAritmetica (valorExp1 + valorExp2)

evalExpAritmetica (MultAritmetica exp1 exp2) contexto = do
  valorExp1 <- evalExpAritmetica exp1 contexto
  valorExp2 <- evalExpAritmetica exp2 contexto
  ExitoAritmetica (valorExp1 * valorExp2)

evalExpAritmetica (ResAritmetica exp1 exp2) contexto = do
  valorExp1 <- evalExpAritmetica exp1 contexto
  valorExp2 <- evalExpAritmetica exp2 contexto
  verificarResAritmetica valorExp1 valorExp2

-- Funciones auxiliares para la evaluación de expresiones aritméticas
verificarResAritmetica :: Int -> Int -> EvalExpAritmetica Int
verificarResAritmetica a b
  | x < 0 = Fallo NoCalculoNatAritmetica
  | otherwise = ExitoAritmetica x
    where x = a - b


buscar :: ExpAritmetica -> Contexto -> Contexto -> EvalExpAritmetica Int
buscar (VarAritmetica c) [] _ = Fallo VariableNoEncontradaAritmetica
buscar (VarAritmetica c) ((id, value):xs) e
  | c == [head id] = ExitoAritmetica value
  | otherwise = buscar (VarAritmetica c) xs e



-- Test para evaluar una expresión numérica
test1 :: EvalExpAritmetica Int
test1 = evalExpAritmetica (NumAritmetica 42) []




-- Test para evaluar una asignación
test2 :: EvalExpAritmetica Int
test2 = evalExpAritmetica (VarAritmetica "x") [("x", 10), ("y", 20)]  --mmmmm rayos 

test3 :: EvalExpAritmetica Int
test3 = evalExpAritmetica (VarAritmetica "x") [("x", 10), ("y", 0)]  --mmmmm rayos 

test4 :: EvalExpAritmetica Int
test4 = evalExpAritmetica (SumaAritmetica (NumAritmetica 5) (VarAritmetica "x")) [("x", 10)]

test5 :: EvalExpAritmetica Int
test5 = evalExpAritmetica (MultAritmetica (NumAritmetica 3) (VarAritmetica "y")) [("y", 7)]

test6 :: EvalExpAritmetica Int
test6 = evalExpAritmetica (ResAritmetica (VarAritmetica "z") (NumAritmetica 4)) [("z", 8)]

test7 :: EvalExpAritmetica Int
test7 = evalExpAritmetica (SumaAritmetica (MultAritmetica (NumAritmetica 2) (VarAritmetica "x")) (ResAritmetica (NumAritmetica 7) (VarAritmetica "y"))) [("x", 5), ("y", 3)]

-- Test para evaluar una variable no encontrada
test8 :: EvalExpAritmetica Int
test8 = evalExpAritmetica (VarAritmetica "z") [("x", 10), ("y", 20)]

-- Test para evaluar una resta que resulta en un número negativo
test9 :: EvalExpAritmetica Int
test9 = evalExpAritmetica (ResAritmetica (NumAritmetica 4) (NumAritmetica 8)) []

-- Test para evaluar una multiplicación que resulta en un número negativo
test10 :: EvalExpAritmetica Int
test10 = evalExpAritmetica (MultAritmetica (NumAritmetica 3) (NumAritmetica (-2))) []


test11 :: EvalExpAritmetica Int
test11 = evalExpAritmetica (NumAritmetica (-10)) []

-- Imprimir los resultados de los tests
main :: IO ()
main = do
  print test1
  print test2
  print test3
  print test4
  print test5
  print test6
  print test7
