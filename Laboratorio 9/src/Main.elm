module Main exposing (..)

--Ejercicio #1 

type Grupo a = 
    Valor a 
    | Suma (Grupo a) (Grupo a)
    | Inverso (Grupo a)


-- Ejercicio #2 

type Algebra t s = Algebra (t -> s) (s -> s-> s) (s -> s) 

iValor1 b =  b
iSuma1 a b = a + b
iInverso1 a = -a
algebra1 = Algebra iValor1 iSuma1 iInverso1


--Ejercicio #3

evaluar : Algebra a s -> Grupo a -> s
evaluar (Algebra iValor iSuma iInverso ) expr =
 let 
    alg = Algebra iValor iSuma iInverso
 in 
    case expr of 
    Valor b -> iValor b 
    Suma a b -> iSuma (evaluar alg a) (evaluar alg b )
    Inverso s -> iInverso (evaluar alg s)
    

--Ejercicio #4

zAlgebra x = 
   let 
      iValor s = s  
      iSuma a b = modBy x (a + b )
      iInverso y = aux x (x-1) y (x-1)
   in  
      Algebra iValor iSuma iInverso  


