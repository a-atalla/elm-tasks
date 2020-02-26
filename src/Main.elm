module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, h1, i, input, li, text, ul)
import Html.Attributes
    exposing
        ( class
        , classList
        , placeholder
        , style
        , target
        , value
        )
import Html.Events exposing (onClick, onInput, onSubmit)



---- MODEL ----


type alias Task =
    { id : Int
    , description : String
    , isComplete : Bool
    }


type alias Model =
    { newTask : Task
    , tasks : List Task
    }


initTask : Task
initTask =
    { id = 0, description = "", isComplete = False }


init : Model
init =
    { newTask = initTask, tasks = [] }



---- UPDATE ----


type Msg
    = TextChange String
    | AddTask
    | ToggleTask Int
    | DeleteTask Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        TextChange text ->
            { model | newTask = { id = model.newTask.id, description = text, isComplete = False } }

        AddTask ->
            let
                oldTask =
                    model.newTask

                newTask =
                    { oldTask | id = oldTask.id + 1 }
            in
            if model.newTask.description == "" then
                model

            else
                { model
                    | tasks =
                        newTask
                            :: model.tasks
                    , newTask = { newTask | description = "" }
                }

        ToggleTask taskId ->
            let
                toggle task =
                    if taskId == task.id then
                        { task | isComplete = not task.isComplete }

                    else
                        task
            in
            { model | tasks = List.map toggle model.tasks }

        DeleteTask taskId ->
            { model | tasks = List.filter (\task -> not (taskId == task.id)) model.tasks }



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        tasks =
            List.reverse model.tasks
    in
    div [ class "container" ]
        [ h1
            [ class "title"
            ]
            [ text "Tasks List" ]
        , form
            [ class "form"
            , onSubmit AddTask
            ]
            [ input
                [ class "input"
                , placeholder "New Task"
                , value model.newTask.description
                , onInput TextChange
                ]
                []
            , button
                [ class "btn-add"
                , onClick AddTask
                ]
                [ i [ class "fas fa-plus-circle fa-2x" ] [] ]
            ]
        , div [] [ ul [] (List.map toLi tasks) ]
        ]


toLi : Task -> Html Msg
toLi task =
    div
        [ class "row item-wrapper" ]
        [ li
            [ classList [ ( "is-complete", task.isComplete ) ]
            , onClick (ToggleTask task.id)
            ]
            [ text task.description ]
        , button
            [ class "delete-btn"
            , onClick (DeleteTask task.id)
            ]
            [ i [ class "far fa-trash-alt fa-2x" ] [] ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
