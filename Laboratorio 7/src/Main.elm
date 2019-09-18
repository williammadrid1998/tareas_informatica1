module Main exposing (..)

--Ejercicio #1


type Arbol = Nil | Cons Int Arbol Arbol

--Ejercicio #2
masUno : Arbol -> Arbol


masUno arbol  = 
    case arbol of
    Nil -> Nil
    Cons a b c -> Cons (a + 1) (masUno b) (masUno c)

--Ejercicio #3
map: (Int -> Int) -> Arbol -> Arbol

map f arbol  = 
    case arbol of
    Nil -> Nil
    Cons a b c -> Cons ( f a) (map f b) (map f c)


--Ejercicio #4
sum : Arbol -> Int


sum arbol =
    case arbol of
    Nil -> 0
    Cons a b c ->  a + (sum b)+(sum c)

--Ejercicio #5

foldTree: (Int->Int -> Int -> Int) -> Int -> Arbol -> Int
foldTree f a arbol =
    case arbol of 
    Nil -> a  
    Cons x y z -> f x (foldTree f a y) (foldTree f a z)





prueba = Cons 4 Nil Nil 
prueba2 = Cons 4 (Cons 5 Nil Nil) Nil
prueba3 x = x*2
prueba4 y = y+1 