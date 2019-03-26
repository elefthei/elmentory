port module Main exposing (..)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>
-}

import Iso8601
import Time
import Browser
import Browser.Dom as Dom
import Html exposing (..)
import File exposing (File)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as D
import Task

main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elmentory • Inventory Control", body = [view model] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }

port setStorage : List Row -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel.entries, cmds ]
        )


-- Boilerplate
type alias Error = String

-- Primary key; product ID
type alias ProductId = Int

type alias Barcode =
    { product: ProductId
    , num: Int
    }

-- Data of each row
type alias Row =
    { distributor : Int
    , date: Int
    , product: ProductId
    , description: String
    , cs_price: Float
    , ea_price: Float
    , ncs: Int
    , nea: Int
    , eprice: Float
    , order: Int
    , is_ordered: Bool
    , is_received: Bool
    , is_used: Bool
    }

type Status = Incoming | Outgoing

type alias Model =
    { entries : List Row
    , field : String
    , files : List File
    , visibility: String
    }

emptyModel : Model
emptyModel =
    { entries = []
    , visibility = "All"
    , field = ""
    , files = []
    }

-- New entry
newEntry : ProductId -> Row
newEntry prod =
    { distributor = 0
    , date = 0
    , product = prod
    , description = "<Placeholder>"
    , cs_price = 0.0
    , ea_price = 0.0
    , ncs = 0
    , nea = 0
    , eprice = 0.0
    , order = 0
    , is_ordered = False
    , is_received = False
    , is_used = False
    }

-- ISO Time
parseTime : String -> Time.Posix
parseTime string =
    Iso8601.toTime string
        |> Result.withDefault (Time.millisToPosix 0)

-- Get description from product ID
getDescription : List Row -> ProductId -> Result Error String
getDescription entries prod =
    case entries of
        (row::tail) ->
            if row.product == prod then
                Ok row.description
            else
                getDescription tail prod
        ([]) -> Err <| "Product ID " ++ String.fromInt prod ++ " not found in inventory"


-- Give the current batch a description for each item
--updateBatch : Model -> Batch ProductId -> Batch (ProductId, String)
--updateBatch model {action, items} =
--    case batch of
init : () -> ( Model, Cmd Msg )
init _ =
  ( emptyModel
  , Cmd.none
  )

-- UPDATE
{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | Import (List File)
    | Scan
    | UpdateField String
    | Delete Int
    | ChangeVisibility String
    | Done


-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        Import files ->
            ({model | files = files} , Cmd.none)
        Scan ->
            ( { model
                | field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries
                    else
                        model.entries ++ [ newEntry << Maybe.withDefault 0 << String.toInt <| model.field ]
              }
            , Cmd.none
            )
        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )
        Delete prod ->
            ( { model | entries = List.filter (\t -> t.product /= prod) model.entries }
            , Cmd.none
            )
        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )
        Done -> -- TODO: SQL integration
            (model, Cmd.none)


-- VIEW
filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)


view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"
        ]
        [ section
            [ class "todoapp" ]
            [ lazy (\_ -> viewUpload) 0
            , lazy viewInput model.field
            , lazy2 viewEntries model.visibility model.entries
            ]
        , infoFooter
        ]

viewUpload : Html Msg
viewUpload =
    section
        [ class "main"
        ]
        [ input
            [ type_ "file"
            , multiple True
            , on "change" (D.map Import filesDecoder)
            ]
            []
        ]

viewInput : String -> Html Msg
viewInput task =
    header
        [ class "header" ]
        [ h1 [] [ text "Products" ]
        , input
            [ class "new-todo"
            , placeholder "Scan some items"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput UpdateField
            , onEnter Scan
            ]
            []
        ]

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                D.succeed msg
            else
                D.fail "not ENTER"
    in
        on "keydown" (D.andThen isEnter keyCode)


-- VIEW ALL ENTRIES
viewEntries : String -> List Row -> Html Msg
viewEntries visibility entries =
        section
            [ class "main"
            ]
            [ Keyed.ul [ class "todo-list" ] <|
                List.map viewKeyedEntry entries
            ]



-- VIEW INDIVIDUAL ENTRIES
viewKeyedEntry : Row -> ( String, Html Msg )
viewKeyedEntry todo =
    ( String.fromInt todo.product, lazy viewEntry todo )


viewEntry : Row -> Html Msg
viewEntry todo =
    li
        []
        [ div
            [ class "view" ]
            [ label
                []
                [ text todo.description ]
            , button
                [ class "destroy"
                , onClick (Delete todo.product)
                ]
                []
            ]
        ]


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
