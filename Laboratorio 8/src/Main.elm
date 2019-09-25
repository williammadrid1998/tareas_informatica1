module Main exposing (..)

-- Ejercicio # 1 

type Arbol x = Nil | Cons x (Arbol x) (Arbol x)

--Ejercicio # 2 
map : (x -> u ) -> (Arbol x) -> (Arbol u)
map f arbol  = 
    case arbol of
    Nil -> Nil
    Cons a b c -> Cons ( f a) (map f b) (map f c)


--Ejercicio # 3
filtrar : (x -> Bool) -> (Arbol x ) -> (List x)
filtrar f arbol = 
    case arbol of
    Nil -> []
    Cons a b c -> if f a then [a]++(filtrar f b)++(filtrar f c)  else (filtrar f b)++(filtrar f c)

--Ejercicio # 4 

foldTree : (x -> x -> x -> x) -> x -> (Arbol x) -> x
foldTree f a arbol =
    case arbol of 
    Nil -> a  
    Cons x y z -> f x (foldTree f a y) (foldTree f a z)


-- Ejercicio # 5
filtrarFoldTree : (x -> Bool ) -> Arbol x -> List x
filtrarFoldTree f arbol  = 
    let 

         arb =  (Cons a b c ) 
         aux  = if f a then [a]++(aux f b)++(aux f c)  else (aux f b)++(aux f c)
       
    in 
        foldTree aux [] arb 