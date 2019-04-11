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
import Set as Set exposing (Set)
import Dict as Dict exposing (Dict)
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
import Json.Decode.Pipeline as DP
import Json.Encode as E
import Tuple
import Task
import Debug as Dbg

main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elmentory • Inventory Control", body = [view model] }
        , update = update
        , subscriptions = subscriptions
        }

-- Write an encoded Row to DB
port db : E.Value -> Cmd msg
port importdb : UID -> Cmd msg
port load : (E.Value -> msg) -> Sub msg

-- Subscribe to DB load
subscriptions : Model -> Sub Msg
subscriptions model =
    load (ImportDB << decode)

-- Print the page
port print : () -> Cmd msg

-- Boilerplate
type alias Error = String

-- Primary key; product ID
type alias UID = Int

-- Scanned barcode type
type alias Barcode =
    { product: UID
    , num: Int
    }

-- Barcode functionality
emptyBarcode : Barcode
emptyBarcode = { product = 0, num = 0 }

-- Parse a barcode string into a barcode object
parseBarcode : String -> Maybe Barcode
parseBarcode prod =
    let
        num = Maybe.withDefault 0 << String.toInt << String.right 2 <| prod
        getProduct = String.toInt << String.slice 6 13
    in
        Just prod
            |> Maybe.andThen getProduct
            |> Maybe.andThen (\id -> Just { product = id, num = num })

-- Now show it in a readable format
showBarcode : Barcode -> String
showBarcode bar = String.fromInt bar.product ++ " #" ++ String.fromInt bar.num

-- Data of each row
type alias Row =
    { distributor : UID
    , date: String
    , order: UID
    , description: String
    , received : Set Int
    , high : Set Int
    , inter: Set Int
    , lower : Set Int
    , academia : Set Int
    , total : Int
    , price: Float
    }

-- Row functionality
emptyRow : Row
emptyRow =
    { distributor = 0
    , date = ""
    , order = 0
    , total = 0
    , high = Set.empty
    , inter = Set.empty
    , lower = Set.empty
    , academia = Set.empty
    , received = Set.empty
    , description = ""
    , price = 0.0
    }

-- Encode a set as a JSON string
encodeSet : Set Int -> E.Value
encodeSet = E.string
            << E.encode 0
            << E.list E.int
            << Set.toList

encodeRow : Row -> E.Value
encodeRow r = E.object
    [ ("distributor", E.int r.distributor)
    , ("date", E.string r.date)
    , ("order", E.int r.order)
    , ("total", E.int r.total)
    , ("lower", encodeSet r.lower)
    , ("inter", encodeSet r.inter)
    , ("high", encodeSet r.high)
    , ("received", encodeSet r.received)
    , ("description", E.string r.description)
    , ("price", E.float r.price)
    ]

encode : Catalog -> E.Value
encode = E.object << Dict.foldr (\k v l -> (String.fromInt k, encodeRow v)::l) []

-- No sets on SQL, use JSON strings instead
decodeSet : String -> Set Int
decodeSet s =
    case D.decodeString (D.list D.int) s of
        (Ok l) -> Set.fromList l
        (Err k) -> Dbg.log (D.errorToString k) Set.empty

type alias IntermediateRow =
    { product: Int
    , distributor: Int
    , date: String
    , order: Int
    , total: Int
    , lower: String -- Not yet parsed the inner list
    , inter: String -- Not yet parsed the inner list
    , high: String -- Not yet parsed the inner list
    , academia: String -- Not yet parsed the inner list
    , received: String -- Not yet parsed the inner list
    , description: String
    , price: Float
    }

-- Use an intermediate representation between parsing steps
decoderIntermediate : D.Decoder IntermediateRow
decoderIntermediate =
    D.succeed IntermediateRow
    |> DP.required "product" D.int
    |> DP.required "distributor" D.int
    |> DP.required "date" D.string
    |> DP.required "order" D.int
    |> DP.required "total" D.int
    |> DP.required "lower" D.string
    |> DP.required "inter" D.string
    |> DP.required "high" D.string
    |> DP.required "academia" D.string
    |> DP.required "received" D.string
    |> DP.required "description" D.string
    |> DP.required "price" D.float

-- Now parse the inner representation to the outer one
parseInner : IntermediateRow -> (UID, Row)
parseInner { product, distributor, date, order, total, lower, inter, high, academia, received, description, price } =
    (product
    , { emptyRow | distributor = distributor
                 , date = date
                 , order = order
                 , total = total
                 , lower = decodeSet lower
                 , inter = decodeSet inter
                 , high = decodeSet high
                 , academia = decodeSet academia
                 , received = decodeSet received
                 , description = description
                 , price = price
      }
    )

-- Decode JSON value to an Elm dictionary
decode : E.Value -> Catalog
decode v =
    case D.decodeValue (D.list decoderIntermediate) v of
        (Ok inter) -> inter
                        |> List.map parseInner
                        |> Dict.fromList
        (Err e) -> Dbg.log (D.errorToString e) Dict.empty

-- Top Data store
type alias Catalog = Dict UID Row

-- Top level model for UI
type alias Model =
    { entries : Catalog
    , field : String
    , files : List File
    , mode : Mode
    }

emptyModel : Model
emptyModel =
    { entries = Dict.empty
    , field = ""
    , files = []
    , mode = Recv
    }

type Mode = High | Inter | Lower | Academia | Recv

-- Row CSV parsing functionality
parseRow : List String -> Maybe (UID, Row)
parseRow attr =
    let
        unsafeInt = Maybe.withDefault 0 << String.toInt
        unsafeFloat = Maybe.withDefault 0.0 << String.toFloat
    in
    case attr of
        (customer::distributor::dept::date::po_num::prod::customer_prod::desc::brand::pack_size::cs_price::ea_price::csn::ean::eprice::ordern::[]) ->
            Just ( unsafeInt prod
                 , { emptyRow
                    | distributor = unsafeInt distributor
                    , date = date
                    , order = unsafeInt ordern
                    , description = desc ++ ", " ++ brand ++ ", " ++ pack_size
                    , total = unsafeInt csn
                    , received = Set.empty
                    , lower = Set.empty
                    , inter = Set.empty
                    , high = Set.empty
                    , academia = Set.empty
                    , price = unsafeFloat eprice
                 })
        (l) -> Nothing

-- JSON decoder
filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)

-- Scan a barcode
scan : Mode -> Barcode -> Catalog -> Catalog
scan mode { product, num } =
    Dict.update product (\m ->
      case (mode, m) of
        (Lower, Just row) -> Just { row | lower = Set.insert num row.lower }
        (Inter, Just row) -> Just { row | inter = Set.insert num row.inter }
        (High, Just row) -> Just { row | high = Set.insert num row.high }
        (Academia, Just row) -> Just { row | academia = Set.insert num row.academia }
        (Recv,  Just row) -> Just { row | received = Set.insert num row.received }
        (_, Nothing) -> Nothing
      )

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
    | ImportDB Catalog
    | Scan
    | ChangeMode Mode
    | UpdateField String
    | Done
    | Print

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
                ((prod,r)::trows) ->
                    ({ model
                        | entries = Dict.fromList ((prod,r)::trows)
                     }
                     , importdb r.order -- Load DB entries for that order
                    )
        ImportDB catalog -> ({ model | entries = Dict.union model.entries catalog }
                            , Cmd.none)
        Scan ->
            let
                barcode =
                    Maybe.withDefault emptyBarcode << parseBarcode <| model.field
            in
            ( { model
                | field = ""
                , entries = scan model.mode barcode model.entries
              }
            , Cmd.none
            )
        Print -> (model, print ())
        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )
        ChangeMode mode ->
            ( { model | mode = mode }
            , Cmd.none
            )
        Done ->
            (model, model.entries
                    |> encode
                    |> db
            )

view : Model -> Html Msg
view model =
    div
        [ class "prodapp-wrapper"]
        [ section
            [ class "prodapp" ]
            [ viewUpload
            , lazy viewInput model.field
            , lazy2 viewControls (List.length << Dict.values <| model.entries) model.mode
            , lazy viewEntries model.entries
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
        [ class "prodapp-main"
        ]
        [ h2 [] [ text "Import order CSV file"
        , input
            [ type_ "file"
            , multiple True
            , on "change" (D.map LoadCsv filesDecoder)
            ]
            []
          ]
        ]

viewInput : String -> Html Msg
viewInput task =
    header
        [ class "header" ]
        [ h1 [] [ text "Products" ]
        , input
            [ class "prodapp-main"
            , placeholder "Scan some items"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput UpdateField
            , onEnter Scan
            , onBlur (UpdateField "")
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
viewEntries : Catalog -> Html Msg
viewEntries entries =
        section
            [ class "main"
            ]
            [ table [ class "prod-table" ]
                (List.concat [
                  [ thead []
                    [ th [] [text "#"]
                    , th [] [text "ProductID"]
                    , th [] [text "Description"]
                    , th [] [text "Received"]
                    , th [] [text "Lower"]
                    , th [] [text "Inter"]
                    , th [] [text "High"]
                    , th [] [text "Academia"]
                    ]
                  ]
                  , entries
                    |> Dict.map viewKeyedEntry
                    |> Dict.values
                ])
            ]

-- VIEW INDIVIDUAL ENTRIES
viewKeyedEntry : UID -> Row -> Html Msg
viewKeyedEntry product row =
    tr [ class "row" ]
       [ td [] [ text (String.fromInt row.total) ]
       , td [] [ text (String.fromInt product) ]
       , td [] [ text row.description ]
       , td [] [ text << String.fromInt << Set.size <| row.received ]
       , td [] [ text << String.fromInt << Set.size <| row.lower ]
       , td [] [ text << String.fromInt << Set.size <| row.inter ]
       , td [] [ text << String.fromInt << Set.size <| row.high ]
       , td [] [ text << String.fromInt << Set.size <| row.academia ]
       ]

viewControls : Int -> Mode -> Html Msg
viewControls nitems mode =
    let
        viewNum n =
            if n == 1 then
                text (String.fromInt n ++ " item ordered")
            else
                text (String.fromInt n ++ " items ordered")
    in
        footer
            [ class "footer"
            ]
            [ span
                [ class "prod-count" ]
                [ strong [] [
                    lazy viewNum nitems
                    ]
                ]
            , lazy viewControlsRecv mode
            ]

viewControlsRecv : Mode -> Html Msg
viewControlsRecv mode =
    ul
        [ class "filters" ]
        [ modeToggle Recv mode, text " "
        , modeToggle Lower mode, text " "
        , modeToggle Inter mode, text " "
        , modeToggle High mode, text " "
        , modeToggle Academia mode, text " "
        , viewDone
        , text " "
        , viewPrint
        ]

viewDone : Html Msg
viewDone =
        button
            [ onClick Done ]
            [ text "Done" ]

viewPrint : Html Msg
viewPrint =
        button
            [ onClick Print ]
            [ text "Print" ]

modeToggle : Mode -> Mode -> Html Msg
modeToggle mode actualm =
    let
        (uri, str) = case mode of
            (Lower) -> ("#/lower", "Lower")
            (Inter) -> ("#/intermediate", "Intermediate")
            (High) -> ("#/highschool", "Highschool")
            (Academia) -> ("#/academia", "Academia")
            (Recv) -> ("#/received", "Received")
    in
        li
            [ onClick (ChangeMode mode) ]
            [ a [ href uri, classList [ ( "selected", mode == actualm ) ] ]
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


