port module Main exposing (main)

-- port module Todo006 exposing (main)
-- this is todo002 revision
-- next mission is to implement localstorage like 21 / 25-elm-examples
-- link to todolocal001.html

import Browser
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE



-- MAIN


type alias Flags =
    { todos : List String }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { text : String
    , todos : List String
    , editing : Maybe TodoEdit
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model "" flags.todos Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateText String
    | AddTodo
    | RemoveTodo Int


type alias TodoEdit =
    { index : Int
    , text : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateText newText ->
            ( { model | text = newText }, Cmd.none )

        AddTodo ->
            let
                newTodos =
                    model.todos ++ [ model.text ]
            in
            ( { model | text = "", todos = newTodos }
            , saveTodos newTodos
            )

        RemoveTodo index ->
            let
                beforeTodos =
                    List.take index model.todos

                afterTodos =
                    List.drop (index + 1) model.todos

                newTodos =
                    beforeTodos ++ afterTodos
            in
            ( { model | todos = newTodos }, saveTodos newTodos )


port saveTodos : List String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div
        [ HA.style "margin" "60px auto"
        , HA.style "width" "400px"
        ]
        [ h1 [] [ text "Enter itmes to do" ]
        , form [ HE.onSubmit AddTodo ]
            [ input
                [ HE.onInput UpdateText
                , HA.value model.text
                , HA.autofocus True
                , HA.style "width" "70%"
                , HA.placeholder "Enter a todo"
                ]
                []
            , button
                [ HA.disabled (String.isEmpty (String.trim model.text))
                , HA.style "width" "27%"
                ]
                [ text "Add Todo" ]
            ]
        , div []
            (List.indexedMap
                (\index todo ->
                    div [ HA.style "padding" "2px" ]
                        [ text todo
                        , span
                            [ HE.onClick (RemoveTodo index)
                            ]
                            [ button [ HA.style "float" "right" ] [ text "Remove" ] ]
                        ]
                )
                model.todos
            )
        ]
