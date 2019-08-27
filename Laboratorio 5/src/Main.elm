module Main exposing (..)

-- ejercicio # 2



esP b a = 
    if b == 2 then True
    else if b == a then True 
    else if modBy a b == 0 then False     
    else esP b (a +1 )
esPrimo : Int -> Bool
esPrimo x = esP x 2 

 
--Ejercicion # 3 :
 
fibonacci n =
    case n of
    0 -> 0
    1 -> 1
    a -> fibonacci (n-1) + fibonacci (n-2) 

-- Ejercicio # 4 

primos : Int -> List Int
primos ns = case ns of 
    0 -> []
    1 -> [1]
    n -> if esPrimo n == True then n :: primos(n-1) else primos(n - 1)


--Ejercicio # 5 


npri a b = 
    if a == 0 then [] 
    else if esPrimo b == False then npri a (b + 1) 
    else b :: npri (a - 1) (b + 1)


nPrimos  ns = npri ns 2 




