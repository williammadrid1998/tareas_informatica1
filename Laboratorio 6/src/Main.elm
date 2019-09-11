module Main exposing (..)


--Ejercicio #1

iFilter n ns = 
    case ns of 
    [] -> []
    b::bs -> if modBy n b /= 0 then b :: iFilter n bs else iFilter n bs 



--Ejercicio #2

-- Funciones para ejercicio 2 
esImpar x = if modBy 2 x /= 0 then True else False 

esPar n = if modBy 2 n == 0 then True else False 


filter f ns =
    case ns of
    [] -> []
    b::bs -> if f b then b:: filter f bs else filter f bs 

--Ejercicio #3

iZipWith ns xs =
    case (ns,xs) of
    ([],hs) -> []
    (rs,[])-> []
    (b::bs,c::cs)-> (b+c)::iZipWith bs cs 


-- Ejercicio # 4 
zipWith f ns xs = 
    case (ns, xs) of
    ([],hs) -> []
    (rs,[])-> []
    (b::bs,c::cs)-> (f b c )::zipWith f bs cs

