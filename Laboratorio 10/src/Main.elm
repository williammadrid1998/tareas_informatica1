
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = 
  { numero : String
  , lista : List (String)
  , resultado : String  

  }


init : Model
init =
  Model "" [] "" 



-- UPDATE


type Msg
  = Input String  
  | Sumar
  | Multiplicar
  | Reset
  | Calcular 




update : Msg -> Model -> Model
update msg model =
  case msg of
    Input num ->
      Model (model.numero ++ num) (model.lista ++ [num]) (model.resultado)


    Sumar  ->
      {numero = model.numero ++ "+", lista = model.lista ++ ["+"], resultado = model.resultado  }

    Multiplicar -> 
      {numero = model.numero ++ "x", lista = model.lista ++ ["x"], resultado = model.resultado  }

    Reset -> 
       Model "" [] "" 

    Calcular -> 
      Model (evaluar model.lista) model.lista (evaluar model.lista)


esOperador x = if x == "+" || x == "*" then True else False


fusionar lista = 
    case lista of 
        [] -> []
        b::c::bs ->  if esOperador b  && esOperador c then 
                        (b ++ c) :: fusionar2 bs 
                     else if not (esOperador b) then 
                         b :: fusionar2 (c :: bs)   
                     else 
                         ([b] ++ [c]) ++ fusionar2 bs 

fusionar2 a = 
  fusionar a |> fusionar 

  


primerValor : List String -> String
primerValor lista = 
    case lista of 
      [] -> ""
      b::bs -> b


  
restoDeLista lista = 
    case lista of 
      [] -> [""]
      b::bs -> bs



stringInt string = 
  String.toInt string |> Maybe.withDefault 0 

listaString a = 
    case a of 
      [] -> ""
      b :: bs -> b ++ listaString bs

multiplicar lista = 
    case lista of 
      [] -> ["0"]
      b::c::bs -> if c == "x" then 
                    multiplicar (String.fromInt(stringInt b * stringInt(primerValor bs)) :: restoDeLista bs) 
                  else 
                    b :: c :: multiplicar bs

sumar lista = 
    case lista of 
      [] -> ["0"]
      b::c::bs -> if c == "+" then 
                    sumar (String.fromInt(stringInt b * stringInt(primerValor bs)) :: restoDeLista bs) 
                  else 
                    b :: c :: sumar bs


evaluar lista = fusionar2 lista |> multiplicar |> sumar |> primerValor

       



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ 
     div [] [ text (listaString model.lista) ]
    , div [] [ text  model.resultado ],
    
    
      div[][
    
        button [ onClick (Input "7") ] [ text "7" ]
        , button [ onClick (Input "8") ] [ text "8" ]
        , button [ onClick (Input "9") ] [ text "9" ]
        , button [ onClick Sumar ] [ text "+" ]    
      ],
      
      div[][
    
        button [ onClick (Input "4") ] [ text "4" ]
        , button [ onClick (Input "5") ] [ text "5" ]
        , button [ onClick (Input "6") ] [ text "6" ]
        , button [ onClick Multiplicar ] [ text "x" ]    
      ],
      
       div[][
    
        button [ onClick (Input "1") ] [ text "1" ]
        , button [ onClick (Input "2") ] [ text "2" ]
        , button [ onClick (Input "3") ] [ text "3" ]
        , button [ onClick Calcular ] [ text "=" ]    
      ],
      
       div[][
    
        button [ onClick (Input "0") ] [ text "0" ]
        , button [ onClick Reset ] [ text "C" ]         
      ]

    ]