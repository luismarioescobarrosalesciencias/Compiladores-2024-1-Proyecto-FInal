--Functores 

{--
Un funtor en Haskell es una clase de tipo que representa
 tipos que pueden ser mapeados sobre mediante una función. La clase Functor se define así:

class Functor f where  
fmap :: (a -> b) -> f a -> f b

Esto significa que si tenemos un tipo f que es una instancia de Functor, podemos aplicar una función a -> b 
a un valor envuelto en f a para obtener un valor envuelto en f b
--}

--Ejemplo con listas 
--Esta funcion 




--PRELIMINARES--------------------------------------------------------------------


{--
fmap es una función que forma parte del tipo de clase de tipo Functor.
 La función fmap permite aplicar una función a los valores encapsulados en una estructura de datos 
sin modificar la estructura misma. Para los tipos de datos que son instancias de Functor, 
fmap es una forma de "mapear" una función sobre esos valores
--}
listaSuma :: [Int ] -> Int -> [Int]
listaSuma l1 n1 = fmap (+n1) l1

--Notacion equivaletne
listaSuma2 :: [Int ] -> Int -> [Int]
listaSuma2 l1 n1 =  (+n1) <$> l1   --fmap y <$> Son equivalentes





cuadrados :: [Int] -> [Int]
cuadrados lista = fmap (\x -> x * x) lista
-- En Haskell, \x -> ... es la forma de definir una función anónima (o lambda) que toma un parámetro x. 
--    Podemos  pensar en \x -> ... como
--    una función que toma un argumento x y realiza alguna operación especificada después de la flecha ->.
--(\x -> x * x) es una función anónima que toma un número x y devuelve su cuadrado, es decir, x * x.



cuadradosDo :: [Int] -> [Int]
cuadradosDo lista = do
  x <- lista
  return (x * x)
--do: notation se utiliza para secuenciar accione
--x <- lista: Esta línea se lee como "para cada elemento x en la lista lista". El operador <- se utiliza para extraer valores de una monada. 
--  En este caso, extrae cada elemento de la lista y asigna ese valor a x.






--La función pure es una función  que forma parte de la clase de tipos Applicative. Su tipo general es:
--pure :: Applicative f => a -> f a
--Esta función toma un valor de tipo a y lo "envuelve" en una estructura aplicativa f. La función pure es utilizada para introducir valores puros en el contexto de una aplicación, 
--  generalmente en el contexto de Functor o Applicative.
--concatenarListas :: [Char] -> [Char] -> [Char]
--concatenarListas lista1 lista2 = pure (lista1 ++ lista2)


combinaciones :: [Int] -> [Char] -> [(Int, Char)]
combinaciones numeros letras = do
  numero <- numeros
  letra <- letras
  pure (numero, letra)




dividir :: Int -> Int -> Maybe Int
dividir x y =
  if y /= 0
    then pure (x `div` y)
    else Nothing


--
-- Ejemplo
-- dividir 8 2  => Just 4
-- dividir 5 0  => Nothing


{-- 
Lo que hace <*> es aplicar una función envuelta en una estructura aplicativa (f (a -> b))
 a un valor envuelto en la misma estructura aplicativa (f a), produciendo un valor envuelto en esa estructura 
 aplicativa (f b). En otras palabras, permite aplicar funciones dentro de contextos aplicativos.
--}

triplicar :: Int -> Int
triplicar x = x * 3



-- Una lista que contiene la función duplicar
listaTriplicar :: [Int -> Int]
listaTriplicar = [triplicar]

-- Aplicar la función triplicar a cada número usando <*>
resultado :: [Int] -> [Int]
resultado listaNumeros = listaTriplicar <*> listaNumeros
--Prueba
--Resultado


data Result a = Exito a | Fallo String deriving (Show)


instance Functor Result where
  fmap f (Exito x) = Exito (f x)
  fmap _ (Fallo err) = Fallo err




instance Applicative Result where
  pure = Exito
  Exito f <*> Exito x = Exito (f x)
  Fallo err <*> _ = Fallo err
  _ <*> Fallo err = Fallo err


instance Monad Result where
  return = Exito
  Exito x >>= f = f x
  Fallo err >>= _ = Fallo err

-- Ejemplo avanzado, usando la monadas result
divide :: Int -> Int -> Result Double
divide x y =
  if y /= 0
    then Exito (fromIntegral x / fromIntegral y)
    else Fallo "Division por 0"







------------------------USANDO MONADAS para hacer una clase de Arbol Binario------------------------



--Funtor
data ArbolBinarioE a = NodoE a (ArbolBinarioE a) (ArbolBinarioE a) | HojaE deriving (Show)

instance Functor ArbolBinarioE where
  fmap _ HojaE = HojaE
  fmap f (NodoE x izquierda derecha) = NodoE (f x) (fmap f izquierda) (fmap f derecha)



arbolDuplicadoFConParametro :: Int -> ArbolBinarioE Int -> ArbolBinarioE Int 
arbolDuplicadoFConParametro x t1 = fmap (* x) t1
-- Prueba: arbolDuplicadoFConParametro 4 (NodoE 1 (NodoE 2 HojaE HojaE) (NodoE 3 HojaE HojaE))
--Resultado: NodoE 4 (NodoE 8 HojaE HojaE) (NodoE 12 HojaE HojaE)




--Applicative y Monad
instance Applicative ArbolBinarioE where



  pure x = NodoE x HojaE HojaE
  HojaE <*> _ = HojaE
  _ <*> HojaE = HojaE
  (NodoE f izqF derF) <*> arbolX = do
    x <- arbolX
    izqResult <- izqF <*> arbolX
    derResult <- derF <*> arbolX
    return (f x)




instance Monad ArbolBinarioE where
  return x = NodoE x HojaE HojaE
  HojaE >>= _ = HojaE
  (NodoE x izquierda derecha) >>= f =
    NodoE y (izquierda >>= f) (derecha >>= f)
    where
      NodoE y _ _ = f x

arbolDuplicadoMConParametro :: Int -> ArbolBinarioE Int -> ArbolBinarioE Int 
arbolDuplicadoMConParametro x t1 = do
  y <- t1
  return (y * x)

--Prueba: arbolDuplicadoMConParametro 4 (NodoE 1 (NodoE 2 HojaE HojaE) (NodoE 3 HojaE HojaE))
--NodoE 4 (NodoE 8 HojaE HojaE) (NodoE 12 HojaE HojaE)


alturaArbol :: ArbolBinarioE a -> ArbolBinarioE Int
alturaArbol HojaE =     return  0
alturaArbol (NodoE _ izquierda derecha) = do
  alturaIzq <- alturaArbol izquierda
  alturaDer <- alturaArbol derecha
  return (1 + max alturaIzq alturaDer)


-- Función para obtener la altura numérica, y quitar la estructura 
obtenerAltura :: ArbolBinarioE Int -> Int
obtenerAltura (NodoE altura _ _) = altura



--obtiene la altura de un arbol  
ejecutarALtura :: ArbolBinarioE a  -> Int
ejecutarALtura t1 = obtenerAltura(alturaArbol t1 )