module Main exposing (..)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (attribute)
import Svg.Attributes exposing (..)
import Svg exposing (..)
import Calculator exposing (..)
import List exposing (..)


-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model = 
                {
                      input: Float
                    , view_num: String
                    , operation: String
                    , flag: Int
                }

init : Model
init =
  {
      input = 0
    , view_num = "0"
    , operation = ""
    , flag = 0
  }


-- UPDATE

addition : Float -> Float -> Float
addition a b = 
                a + b

subtract : Float -> Float -> Float
subtract a b = 
                a - b

multiply : Float -> Float -> Float
multiply a b = 
                a * b

divide : Float -> Float -> Float
divide a b = 
                a / b

onclickEqual : Model -> String -> Model
onclickEqual model op = 
                        if op /= "" then
                          model
                        else
                          {
                            model |
                            view_num = String.fromFloat model.input
                          }

compute : Model -> Msg -> Model
compute model msg = 
            let 
                val = (Maybe.withDefault 0 (String.toFloat model.view_num))
                new_op = 
                          case msg of
                              Add ->
                                "+"
                              Subtract ->
                                "-"
                              Multiply ->
                                "x"
                              Divide ->
                                "/"
                              _ ->
                                ""
            in
            case model.operation of
              "+" ->
                onclickEqual
                (
                  {
                    model |
                    input = addition model.input val
                  ,  operation = new_op
                  ,  flag = 1
                  }
                ) new_op
              "-" ->
                onclickEqual
                (
                  {
                    model |
                    input = subtract model.input val
                  ,  operation = new_op
                  ,  flag = 1
                  }
                ) new_op
              "x" ->
                onclickEqual
                (
                  {
                    model |
                    input = multiply model.input val
                  ,  operation = new_op
                  ,  flag = 1
                  }
                ) new_op
              "/" ->
                onclickEqual
                (
                  {
                    model |
                    input = divide model.input val
                  ,  operation = new_op
                  ,  flag = 1
                  }
                ) new_op
              _ ->
                onclickEqual
                (
                  {
                    model |
                    operation = new_op
                  , input = Maybe.withDefault 0 (String.toFloat model.view_num)
                  , flag = 1
                  }
                ) new_op




update : Msg  -> Model -> Model
update msg model =
  case msg of
    Clear ->
      init
    Input a ->
      let 
        view_txt = 
                    if model.flag == 1 then
                      a
                    else if model.view_num == "0" && a /= "." then
                      a
                    else 
                      model.view_num ++ a
        flg = 
              if view_txt == a then
                0
              else
                model.flag
      in
      {
        model |
        view_num = view_txt
      , flag = flg
      }
    _ ->
      compute model msg

stylesheet : Html msg
stylesheet =
    let
        tag =
            "link"

        attrs =
            [ Html.Attributes.attribute "Rel" "stylesheet"
            , Html.Attributes.attribute "property" "stylesheet"
            , Html.Attributes.attribute "href" "style.css"
            ]

        children =
            []
    in
        Html.node tag attrs children


view : Model -> Html (Msg)
view model =
              let
                  button_strings = ["1", "2", "3", "+", "4", "5", "6", "-", "7", "8", "9", "x", "0", ".", "=", "/"]

                  viewBox_height = 
                                     ((button_strings
                                  |> length
                                  |> toFloat) / 4
                                  |> ceiling) * 90
                                  |> String.fromInt
              in
              div([Html.Attributes.style "padding-left" "11cm"]++[Html.Attributes.style "padding-top" "2cm"])
              [ 
                  stylesheet
                 ,svg
                 [  viewBox "0 0 400 400"
                 ,  width "400"
                 ,  height viewBox_height
                 ]
                 (
                      get_calculator button_strings
                  ++  [text_
                          [
                             x "350"
                          , y "40"
                          , fill "White"
                          , textAnchor "middle"
                          , fontSize "25"
                          , dominantBaseline "central"
                          , textAnchor "end"
                          ]
                          [text ((model.view_num))]
                      ]
                 )
               ]