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
import List.Extra as LE
import Csv exposing (..)
import Html.Lazy exposing (..)
import Json.Decode as D
import Task
import Debug as Dbg

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
type alias UID = Int

-- Count stats for each unit
type alias Counter =
    { used : Int
    , recv : Int
    , total: Int
    }

emptyCounter : Counter
emptyCounter =
    { used = 0
    , recv = 0
    , total = 0
    }

newCounter : Int -> Counter
newCounter n = { emptyCounter | total = n }

-- Scanned barcode type
type alias Barcode =
    { product: UID
    , num: Int
    }

-- Barcode functionality
emptyBarcode : Barcode
emptyBarcode = { product = 0, num = 0 }

parseBarcode : String -> Maybe Barcode
parseBarcode prod =
    let
        num = Maybe.withDefault 0 << String.toInt << String.right 2 <| prod
        getProduct = String.toInt << String.slice 6 13
    in
        Just prod
            |> Maybe.andThen getProduct
            |> Maybe.andThen (\id -> Just { product = id, num = num })

showBarcode : Barcode -> String
showBarcode bar = String.fromInt bar.product ++ " #" ++ String.fromInt bar.num

fromBarcode : Barcode -> Row
fromBarcode barcode =
    { distributor = 0
    , date = 0
    , order = 0
    , product = barcode.product
    , description = String.fromInt barcode.product
    , counter = emptyCounter
    , price = 0.0
    }

-- Data of each row
type alias Row =
    { distributor : UID
    , date: Int
    , order: UID
    , product: UID
    , description: String
    , counter : Counter
    , price: Float
    }

-- Row functionality
emptyRow : Row
emptyRow =
    { distributor = 0
    , date = 0
    , order = 0
    , product = 0
    , description = ""
    , counter = emptyCounter
    , price = 0.0
    }

-- Increase the counter in this row by 1
incRow : Bool -> Row -> Row
incRow isRecv r =
    let
        incfunc =
            if isRecv then
                (\c -> { c | recv = c.recv + 1 })
            else
                (\c -> { c | used = c.used + 1 })
    in
        { r | counter = incfunc r.counter }

-- Decrease the counter in this row by 1
decRow : Bool -> Row -> Row
decRow isRecv r =
    let
        decfunc =
            if isRecv then
                (\c -> { c | recv =
                    if (c.recv == 0 || c.recv == c.used)
                        then c.recv
                        else c.recv - 1 })

            else
                (\c -> { c | used =
                    if (c.used == 0)
                        then c.used
                        else c.used - 1 })
    in
        { r | counter = decfunc r.counter }

type alias Model =
    { entries : List Row
    , field : String
    , files : List File
    , visibility : String
    , isRecv : Bool
    }

emptyModel : Model
emptyModel =
    { entries = []
    , visibility = "Incoming"
    , field = ""
    , files = []
    , isRecv = True
    }

-- Row CSV parsing functionality
parseRow : List String -> Maybe Row
parseRow attr =
    let
        unsafeInt = Maybe.withDefault 0 << String.toInt
        unsafeFloat = Maybe.withDefault 0.0 << String.toFloat
    in
    case attr of
        (customer::distributor::dept::date::po_num::prod::customer_prod::desc::brand::pack_size::cs_price::ea_price::csn::ean::eprice::ordern::[]) ->
            Just { emptyRow
                    | distributor = unsafeInt distributor
                    , date = Time.posixToMillis (parseTime date)
                    , order = unsafeInt ordern
                    , product = unsafeInt prod
                    , description = desc ++ ", " ++ brand ++ ", " ++ pack_size
                    , counter = newCounter (unsafeInt csn)
                    , price = unsafeFloat eprice
                 }
        (l) -> Nothing

-- JSON decoder
filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)

-- ISO Time
parseTime : String -> Time.Posix
parseTime string =
    Iso8601.toTime string
        |> Result.withDefault (Time.millisToPosix 0)

-- Scan a barcode received or used
scan : Bool -> Barcode -> List Row -> List Row
scan isRecv code =
    LE.updateIf (\r -> r.product == code.product) (incRow isRecv)

-- Unscan removed a scanned item count
unscan : Bool -> UID -> List Row -> List Row
unscan isRecv prod =
    LE.updateIf (\r -> r.product == prod) (decRow isRecv)

-- Elm main app
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
    | LoadCsv (List File)
    | ImportCsv Csv
    | Scan
    | Unscan UID
    | ChangeMode
    | UpdateField String
    | Done

-- Csv is a monoid
concatCsv : Csv -> Csv -> Csv
concatCsv left right = { left | records = left.records ++ right.records }

-- Concat monoid
flattenCsvs : List Csv -> Csv
flattenCsvs csvs =
    case csvs of
        (h::ts) -> concatCsv h << flattenCsvs <| ts
        ([]) ->  { headers = [], records = [] }

-- From list of maybe to List, drops Nothing
maybeCombine : List (Maybe a) -> List a
maybeCombine l =
    case l of
        ((Just a)::ts) -> a :: maybeCombine ts
        (Nothing::ts) -> maybeCombine ts
        ([]) -> []

-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        LoadCsv files ->
            ( model
            , Task.perform ImportCsv (
                    Task.map (flattenCsvs << List.map Csv.parse)
                    << Task.sequence
                    << List.map File.toString
                    <| files)
            )
        ImportCsv csv ->
            case (maybeCombine << List.map parseRow <| csv.records) of
                ([]) ->
                    (model, Cmd.none)
                (rows) ->
                    ({ model
                        | entries = model.entries ++ rows
                     }, Cmd.none )
        Scan ->
            let
                barcode =
                    Maybe.withDefault emptyBarcode << parseBarcode <| model.field
            in
            ( { model
                | field = ""
                , entries = scan model.isRecv barcode model.entries
              }
            , Cmd.none
            )
        Unscan prod ->
            ( { model
                | entries = unscan model.isRecv prod model.entries
              }
            , Cmd.none
            )
        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )
        ChangeMode ->
            ( { model | isRecv = not model.isRecv }
            , Cmd.none
            )
        Done -> -- TODO: SQL integration
            (model, Cmd.none)


-- VIEW
view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"]
        [ section
            [ class "todoapp" ]
            [ viewUpload
            , lazy viewInput model.field
            , lazy viewEntries model.entries
            , lazy2 viewControls (List.length model.entries) model.isRecv
            ]
        , infoFooter
        ]

viewError : Error -> Html Msg
viewError e =
    div
        [ class "error"]
        [ section
            []
            [ text ("An error has occured: " ++ e) ]
        ]

viewUpload : Html Msg
viewUpload =
    section
        [ class "new-todo"
        ]
        [ h3 [] [ text "Import order CSV" ]
        , input
            [ type_ "file"
            , multiple True
            , on "change" (D.map LoadCsv filesDecoder)
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
viewEntries : List Row -> Html Msg
viewEntries entries =
        section
            [ class "main"
            ]
            [ Keyed.ul [ class "todo-list" ] <|
                List.map viewKeyedEntry entries
            ]

-- VIEW INDIVIDUAL ENTRIES
viewKeyedEntry : Row -> (String, Html Msg)
viewKeyedEntry row =
    (String.fromInt row.product
    , tr []
         [ td [] [ text (String.fromInt 1)]
         , td [] [ text row.description ]
         , td [] [ text (String.fromInt row.counter.used) ]
         , td [] [ text (String.fromInt row.counter.recv) ]
         , td [] [ text (String.fromInt row.counter.total) ]
         , td [] [ button [ onClick (Unscan row.product) ] [ text "-" ] ]
         ]
    )

viewControls : Int -> Bool -> Html Msg
viewControls nitems isRecv =
    let
        viewNum n =
            if n == 1 then
                text (String.fromInt n ++ " item scanned")
            else
                text (String.fromInt n ++ " items scanned")
    in
        footer
            [ class "footer"
            ]
            [ span
                [ class "todo-count" ]
                [ strong [] [
                    lazy viewNum nitems
                    ]
                ]
            , lazy viewControlsRecv isRecv
            ]

viewControlsRecv : Bool -> Html Msg
viewControlsRecv isRecv =
    ul
        [ class "filters" ]
        [ modeToggle True isRecv
        , text " "
        , modeToggle False isRecv
        , text " "
        , viewDone
        ]

viewDone : Html Msg
viewDone =
    li
        []
        [ button
            [ onClick Done ]
            [ text "Done" ]
        ]

modeToggle : Bool -> Bool -> Html Msg
modeToggle isRecv actualb =
    let
        uri = if isRecv then "#/received" else "#/outgoing"
        str = if isRecv then "Incoming" else "Used"
    in
        li
            [ onClick ChangeMode ]
            [ a [ href uri, classList [ ( "selected", isRecv == actualb ) ] ]
                [ text str ]
            ]

infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p []
            [ text "Written by "
            , a [ href "https://github.com/elefthei" ] [ text "Lef Ioannidis" ]
            ]
        ]


