module Calculator exposing (..)
import Html exposing (Html, div, span)
import Html.Attributes exposing (attribute)
import Html.Attributes.Aria exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing(..)
import Html.Events exposing (onClick)




type Msg  =     Add 
              | Subtract
              | Multiply
              | Divide
              | Viewtext
              | Clear
              | Input String

button_strings = ["1", "2", "3", "+", "4", "5", "6", "-", "7", "8", "9", "x", "0", ".", "=", "/"]
new_list = List.indexedMap 
                (\ i x -> (get_button i x (25+(90*(modBy 4 i))) (100+(60*(i // 4))))) button_strings

viewBox_height = 
                     ((button_strings
                  |> length
                  |> toFloat) / 4
                  |> ceiling) * 90
                  |> String.fromInt


top_box = [ 

            rect
              [ x "0"
              , y "0"
              , width "400"
              , height viewBox_height
              , fill "#9dd2ea"
              ]
              []

         
          , rect
            [
               x "100"
             , y "10"
             , width "280"
             , height "60"
             , rx "15"
             , ry "15"
             , class "screen"
            ]
            []    
            
        , rect
            [
               x "15"
             , y "17"
             , width "70"
             , height "48"
             , class "clear"
             , onClick Clear
            ]
            []
         , text_
            [
               x "50"
            , y "40"
            , fill "White"
            , textAnchor "middle"
            , fontSize "25"
            , dominantBaseline "central"
            ]
            [text "C"]
     ]

final_list = List.foldl (\ x a -> a++x) top_box new_list

get_button: Int -> String -> Int -> Int -> List (Svg Msg)
get_button index name x_cord y_cord =
           let
            msg = 
                 case index of
                    14 ->
                      Viewtext
                    13 ->
                      Input "."
                    12 ->
                      Input "0"
                    _ ->
                      if modBy 4 (index+1) /= 0 then
                        Input (String.fromInt (index + 1 - ((index)//4)))
                      else
                        case (index+1)//4 of
                          1 ->
                              Add
                          2 ->
                              Subtract
                          3 ->
                              Multiply
                          _ ->
                              Divide
            btn_class = 
                        if index == 14 then
                          "equal"
                        else if modBy 4 (index+1) /= 0 then
                          "btn"
                        else 
                          "operator"             
           in
           [ rect
                  [
                     x (String.fromInt x_cord)
                   , y (String.fromInt y_cord)
                   , width "70"
                   , height "48"
                   , class btn_class
                   , onClick msg
                  ]
                  []
              ,  text_
                   [
                     x (String.fromInt (x_cord+35))
                   , y (String.fromInt (y_cord+24))
                   , fill "Black"
                   , textAnchor "middle"
                   , dominantBaseline "central"
                   , fontSize "25"
                   ]
                   [text name]
            ]
           
calculator : List (Svg Msg)
calculator = 
              final_list