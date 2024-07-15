module Main exposing (main)

import Browser
import Browser.Navigation exposing (Key)
import Element exposing (Element, centerX, column, layout, paragraph, row, text)
import Element.Background
import Element.Border
import Element.Events
import Element.Font as Font
import Element.Input as Input exposing (placeholder)
import Html.Events exposing (on)
import Json.Decode as Decode
import Url exposing (Url)


initialTask : Task
initialTask =
    { id = 0
    , name = ""
    , completed = False
    }


type alias Task =
    { id : Int
    , name : String
    , completed : Bool
    }


type alias Model =
    { key : Key
    , url : Url
    , taskList : List Task
    , newTask : Task
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | NewTaskInput String
    | SubmitForm
    | TaskClicked Int


init : () -> Url.Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, url = url, newTask = { name = "", id = 0, completed = False }, taskList = [] }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked _ ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )

        NewTaskInput taskName ->
            ( { model | newTask = { name = taskName } }, Cmd.none )

        SubmitForm ->
            ( { model | newTask = { name = "" }, taskList = model.taskList |> List.append [ Task model.newTask.id + 1 model.newTask False ] }, Cmd.none )

        TaskClicked taskId ->
            let
                checkedTasks =
                    List.map
                        (\task ->
                            if task.id == taskId then
                                { task | completed = not task.completed }

                            else
                                task
                        )
                        model.taskList
            in
            ( { model | taskList = checkedTasks }, Cmd.none )


taskList : List Task -> Element Msg
taskList list =
    column [ Element.spacingXY 0 10, Element.width Element.fill ]
        (case List.length list of
            0 ->
                [ paragraph [ Font.center ] [ text "Lista vazia, coloque alguma coisa mulher!" ] ]

            _ ->
                List.reverse list
                    |> List.map
                        taskElement
        )


taskElement : Task -> Element Msg
taskElement task =
    row
        [ Element.Border.dashed
        , Element.Border.width 1
        , Element.width Element.fill
        , Element.paddingXY 5 10
        , Element.Events.onClick (TaskClicked task.id)
        , Element.pointer
        ]
        [ paragraph
            (if task.completed then
                [ Font.center, Font.strike ]

             else
                [ Font.center ]
            )
            [ text task.name ]
        ]


mainScreen : Model -> Element Msg
mainScreen model =
    column [ centerX, Element.centerY, Element.spacingXY 0 10 ]
        [ paragraph [ Element.paddingXY 0 30, Font.size 50, Font.bold, Font.color (Element.fromRgb255 { red = 250, green = 115, blue = 3, alpha = 1 }) ] [ text "Inclua sua tarefa" ]
        , taskList model.taskList
        , newTaskForm model.newTask
        ]


newTaskForm : Task -> Element Msg
newTaskForm newTask =
    Input.text [ Element.Border.width 1, Element.Border.solid, onEnter SubmitForm, Font.color (Element.fromRgb255 { red = 0, green = 0, blue = 0, alpha = 1 }) ]
        { label = Input.labelHidden "Tarefa"
        , onChange = NewTaskInput
        , placeholder = Just (placeholder [] (text "Tarefa a se fazer"))
        , text = newTask.name
        }


view : Model -> Browser.Document Msg
view model =
    { title = "a"
    , body =
        [ layout
            [ Font.color (Element.fromRgb255 { red = 255, green = 255, blue = 255, alpha = 1 }), Element.Background.image "https://c4.wallpaperflare.com/wallpaper/596/554/439/nature-white-flowers-green-background-plants-wallpaper-preview.jpg" ]
            --
            (mainScreen model)
        ]
    }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )
