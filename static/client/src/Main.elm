module Main exposing (main)


import Axis
import DateFormat
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Time
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))
import Json.Decode as Decode
import Json.Decode.Extra as JDE
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List
import Task


--The Main Elm part
type Model
  = Failure
  | Loading
  | Success (List Data)

type Msg
  = Again
  | GotData (Result Http.Error (List Data))

type alias Data = 
    {
    time : Time.Posix,
    value : Int
    }

init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getData)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Again ->
      (Loading, getData)
    GotData result ->
      case result of
        Ok data ->
          (Success data, Cmd.none)
        Err _ ->
          (Failure, Cmd.none)

view : Model -> Svg msg
view model = 
    case model of
        Failure ->
            TypedSvg.Core.text "Error: couldnt load"
        Loading ->
            TypedSvg.Core.text "Loading..."
        Success data ->
            svg [ viewBox 0 0 w h ]
                [ TypedSvg.style [] [ TypedSvg.Core.text """
                    .column rect { fill: rgba(118, 214, 78, 0.8); }
                    .column text { display: none; }
                    .column:hover rect { fill: rgb(118, 214, 78); }
                    .column:hover text { display: inline; }
                """ ]
                , g [ transform [ Translate (padding - 1) (h - padding) ] ]
                    [ xAxis (convert data) ]
                , g [ transform [ Translate (padding - 1) padding ] ]
                    [ yAxis ]
                , g [ transform [ Translate padding padding ], TypedSvg.Attributes.class [ "series" ] ] <|
                    List.map (column (xScale (convert data))) (convert data)
                ]

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

--Json dekoder and data conversion
dataDecoder : Decode.Decoder Data
dataDecoder =
    Decode.map2 Data (Decode.field "time" JDE.datetime) (Decode.field "value" Decode.int)


theDecoder : Decode.Decoder (List Data)
theDecoder =
    Decode.list dataDecoder

get : {c | time : Time.Posix, value : Int} -> (Time.Posix, Float)
get {time, value} = (time ,toFloat value)

convert : List({c | time : Time.Posix, value : Int}) -> (List (Time.Posix, Float))
convert data = List.map get data

getData : Cmd Msg
getData =
  Http.get
    { url = "http://localhost:3000/data/1"
    , expect = Http.expectJson GotData theDecoder
    }

--Chart
w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : List ( Time.Posix, Float ) -> BandScale Time.Posix
xScale model =
    List.map Tuple.first model
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * padding )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 600 )


dateFormat : Time.Posix -> String
dateFormat =
    DateFormat.format [ DateFormat.dayOfMonthFixed, DateFormat.text " ", DateFormat.monthNameAbbreviated, 
                        DateFormat.text " - ", DateFormat.hourMilitaryFixed, DateFormat.text ":", DateFormat.minuteFixed] Time.utc


xAxis : List ( Time.Posix, Float ) -> Svg msg
xAxis model =
    Axis.bottom [] (Scale.toRenderable dateFormat (xScale model))


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale


column : BandScale Time.Posix -> ( Time.Posix, Float ) -> Svg msg
column scale ( date, value ) =
    g [ TypedSvg.Attributes.class [ "column" ] ]
        [ rect
            [ x <| Scale.convert scale date
            , y <| Scale.convert yScale value
            , TypedSvg.Attributes.InPx.width <| Scale.bandwidth scale
            , TypedSvg.Attributes.InPx.height <| h - Scale.convert yScale value - 2 * padding
            ]
            []
        , text_
            [ x <| Scale.convert (Scale.toRenderable dateFormat scale) date
            , y <| Scale.convert yScale value - 5
            , textAnchor AnchorMiddle
            ]
            [ TypedSvg.Core.text <| String.fromFloat value ]
        ]
