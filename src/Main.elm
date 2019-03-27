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
import Csv exposing (..)
import Html.Lazy exposing (..)
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
    , csn: Int
    , ean: Int
    , eprice: Float
    , order: Int
    , is_ordered: Bool
    , is_received: Bool
    , is_used: Bool
    }

type alias Model =
    { scanned : List Barcode
    , entries : List Row
    , field : String
    , files : List File
    , visibility : String
    , errormsg : Error
    }

emptyModel : Model
emptyModel =
    { scanned = []
    , entries = []
    , visibility = "All"
    , field = ""
    , files = []
    , errormsg = ""
    }

-- Row functionality
emptyRow : Row
emptyRow =
    { distributor = 0
    , date = 0
    , product = 0
    , description = ""
    , cs_price = 0.0
    , ea_price = 0.0
    , csn = 0
    , ean = 0
    , eprice = 0.0
    , order = 0
    , is_ordered = False
    , is_received = False
    , is_used = False
    }

-- Barcode functionality
emptyBarcode : Barcode
emptyBarcode = { product = 0, num = 0 }

parseBarcode : String -> Maybe Barcode
parseBarcode prod =
    let
        num = Maybe.withDefault 0 << String.toInt << String.right 2 <| prod
        getProduct = String.toInt << String.slice 5 12
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
    , product = barcode.product
    , description = String.fromInt barcode.product
    , cs_price = 0.0
    , ea_price = 0.0
    , csn = 0
    , ean = 0
    , eprice = 0.0
    , order = 0
    , is_ordered = True
    , is_received = True
    , is_used = False
    }

-- Row functionality
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
                    , product = unsafeInt prod
                    , description = desc ++ ", " ++ brand ++ ", " ++ pack_size
                    , cs_price = unsafeFloat cs_price
                    , ea_price = unsafeFloat ea_price
                    , csn = unsafeInt csn
                    , ean = unsafeInt ean
                    , eprice = unsafeFloat eprice
                    , order = unsafeInt ordern
                    , is_ordered = True
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
    | LoadCsv (List File)
    | ImportCsv Csv
    | Scan
    | UpdateField String
    | Delete Int
    | ChangeVisibility String
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
                    ({ model
                        | errormsg = "No valid rows found in CSV file" ++ Debug.toString csv
                     }, Cmd.none )
                (rows) ->
                    ({ model
                        | entries = model.entries ++ rows
                     }, Cmd.none )
        Scan ->
            ( { model
                | field = ""
                , scanned =
                    if String.isEmpty model.field then
                        model.scanned
                    else
                        model.scanned ++ [ Maybe.withDefault emptyBarcode << parseBarcode <| model.field ]
              }
            , Cmd.none
            )
        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )
        Delete prod ->
            ( { model | scanned = List.filter (\t -> t.product /= prod) model.scanned }
            , Cmd.none
            )
        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )
        Done -> -- TODO: SQL integration
            (model, Cmd.none)


-- VIEW
view : Model -> Html Msg
view model =
    if model.errormsg /= "" then viewError model.errormsg
    else
        div
            [ class "todomvc-wrapper"]
            [ section
                [ class "todoapp" ]
                [ text model.visibility
                , viewUpload
                , lazy viewInput model.field
                , h3 [] [ text "Scanned items" ]
                , lazy viewScanned model.scanned
                , h3 [] [ text "Ordered items" ]
                , lazy viewEntries model.entries
                , lazy3 viewControls model.visibility model.scanned model.entries
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

-- VIEW Scanned items
viewScanned : List Barcode -> Html Msg
viewScanned barcodes =
         section
            [ class "scanned"
            ]
            [ Keyed.ul [ class "todo-list" ] <|
                List.map viewScannedEntry barcodes
            ]

-- VIEW SCANNED Entries
viewScannedEntry : Barcode -> (String, Html Msg)
viewScannedEntry barcode =
    let
        barstr = showBarcode barcode
    in
    (barstr,
      li
        []
        [ div
            [ class "view" ]
            [ label
                []
                [ text barstr ]
             , button
                [ class "destroy"
                , onClick (Delete barcode.product)
                ]
                []
            ]
        ])


-- VIEW INDIVIDUAL ENTRIES
viewKeyedEntry : Row -> (String, Html Msg)
viewKeyedEntry row =
    (String.fromInt row.product,
      li
        []
        [ div
            [ class "view" ]
            [ label
                []
                [ text (String.fromInt row.product ++ " - " ++ row.description) ]
            ]
        ])

viewControls : String -> List Barcode -> List Row -> Html Msg
viewControls visibility barcodes entries =
    let
        codesn = List.length (barcodes)
        entriesn = List.length (entries)
    in
        footer
            [ class "footer"
            ]
            [ lazy2 viewControlsCount codesn entriesn
            , lazy viewControlsFilters visibility
            ]


viewControlsCount : Int -> Int -> Html Msg
viewControlsCount scannedNum entriesNum =
    let
        item_ =
            if scannedNum == 1 then
                " item"
            else
                " items"
    in
        span
            [ class "todo-count" ]
            [ strong [] [
                text (String.fromInt scannedNum ++ "/" ++ String.fromInt entriesNum)
            ]
            , text (item_ ++ " scanned")
            ]


viewControlsFilters : String -> Html Msg
viewControlsFilters visibility =
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" "All" visibility
        , text " "
        , visibilitySwap "#/in" "Incoming" visibility
        , text " "
        , visibilitySwap "#/out" "Outgoing" visibility
        ]


visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
    li
        [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text visibility ]
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


