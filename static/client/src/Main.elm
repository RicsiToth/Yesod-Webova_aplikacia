module Main exposing (main)


import Axis
import DateFormat
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import Time
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))
import Json.Decode as Decode
import Json.Decode.Extra as JDE
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events
import Http
import List
import String


--The Main Elm part
type alias Model
  = (String, Model2)

type Model2 
  = Failure 
  | Loading 
  | Success (List Data)

type Msg
  = Again
  | GotData (Result Http.Error (List Data))

type alias Data = 
    {
    time : Time.Posix,
    value : Float
    }

init : String -> (Model, Cmd Msg)
init devId =
  ((devId, Loading), getData devId)

update : Msg -> Model -> (Model, Cmd Msg)
update msg (devId, _) =
  case msg of
    Again ->
      ((devId, Loading), getData devId)
    GotData result ->
      case result of
        Ok data ->
          ((devId ,Success data), Cmd.none)
        Err _ ->
          ((devId, Failure), Cmd.none)

view : Model -> Html Msg
view model = 
    case model of
        (_, Failure) ->
            div [ Html.Attributes.class "graph_fail" ]
                [ button [ Html.Events.onClick Again, Html.Attributes.style "display" "block" ] [ Html.text "Refresh" ]
                , Html.text "Error: couldnt load"
                ]
        (_, Loading) ->
            Html.text "Loading..."
        (_, Success data) ->
            div [ Html.Attributes.class "graph" ]
                [ button [ Html.Events.onClick Again, Html.Attributes.style "display" "block" ] [ Html.text "Refresh" ]
                , div [] [ Html.p [ Html.Attributes.class "average" ] [ Html.text ("Average is " ++ String.fromFloat (average (convert data))) ] ]
                , svg [ viewBox 0 0 w h ]
                    [ TypedSvg.style [] [ TypedSvg.Core.text """
                        .column rect { fill: white; }
                        .column text { display: none; }
                        .column:hover rect { fill: royalblue; }
                        .column:hover text { display: inline; }
                    """ ]
                    , g [ transform [ Translate (padding - 1) (h - padding) ] ]
                        [ xAxis (convert data) ]
                    , g [ transform [ Translate (padding - 1) padding ] ]
                        [ yAxis (convert data)]
                    , g [ transform [ Translate padding padding ], TypedSvg.Attributes.class [ "series" ] ] <|
                        List.map (column (xScale (convert data)) (yScale (convert data))) (convert data)
                    ]
                ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main : Program String Model Msg
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
    Decode.map2 Data (Decode.field "time" JDE.datetime) (Decode.field "value" Decode.float)


theDecoder : Decode.Decoder (List Data)
theDecoder =
    Decode.list dataDecoder

get : {c | time : Time.Posix, value : Float} -> (Time.Posix, Float)
get {time, value} = (time , value)

convert : List({c | time : Time.Posix, value : Float}) -> (List (Time.Posix, Float))
convert data = List.map get data

getData : String -> Cmd Msg
getData devId =
  Http.get
    { url = String.append "http://localhost:3000/data/" devId
    , expect = Http.expectJson GotData theDecoder
    }

--Chart
w : Float
w =
    900


h : Float
h =
    400


padding : Float
padding =
    30

maximum : List ( Time.Posix, Float ) -> Float
maximum list =
    case list of
        [] -> 0
        [x] -> Tuple.second x
        (x::xs) ->
            let 
                maxTail = maximum xs
                value = Tuple.second x
            in
                if value > maxTail then
                    value
                else
                    maxTail


minimum : List ( Time.Posix, Float ) -> Float
minimum list =
    case list of
        [] -> 0
        [(_, y)] -> y
        ((_, y)::xs) ->
            let 
                minTail = minimum xs
            in
                if y < minTail then
                    y
                else
                    minTail


getFloatFromList : List ( Time.Posix, Float ) -> List Float
getFloatFromList list = 
    case list of
       [] -> []
       (x::xs) -> (Tuple.second x :: getFloatFromList xs) 


average : List ( Time.Posix, Float ) -> Basics.Float
average list =
    let
        listFl = getFloatFromList list
    in
        List.sum listFl / Basics.toFloat (List.length listFl)


xScale : List ( Time.Posix, Float ) -> BandScale Time.Posix
xScale model =
    List.map Tuple.first model
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * padding )


yScale : List ( Time.Posix, Float ) -> ContinuousScale Float
yScale model =
    let 
        max = maximum model
        min = minimum model
    in
        Scale.linear ( h - 2 * padding, 0 ) ( min, max )


dateFormat : Time.Posix -> String
dateFormat =
    DateFormat.format [ DateFormat.dayOfMonthFixed, DateFormat.text " ", DateFormat.monthNameAbbreviated, 
                        DateFormat.text " - ", DateFormat.hourMilitaryFixed, DateFormat.text ":", DateFormat.minuteFixed] Time.utc


xAxis : List ( Time.Posix, Float ) -> Svg msg
xAxis model =
    Axis.bottom [] (Scale.toRenderable dateFormat (xScale model))


yAxis : List ( Time.Posix, Float ) -> Svg msg
yAxis model =
    Axis.left [ Axis.tickCount 10 ] (yScale model)


column : BandScale Time.Posix -> ContinuousScale Float -> ( Time.Posix, Float ) -> Svg msg
column xscale yscale ( date, value ) =
    g [ TypedSvg.Attributes.class [ "column" ] ]
        [ rect
            [ x <| Scale.convert xscale date
            , y <| Scale.convert yscale value
            , TypedSvg.Attributes.InPx.width <| Scale.bandwidth xscale
            , TypedSvg.Attributes.InPx.height <| h - Scale.convert yscale value - 2 * padding
            ]
            []
        , text_
            [ x <| Scale.convert (Scale.toRenderable dateFormat xscale) date
            , y <| Scale.convert yscale value - 5
            , textAnchor AnchorMiddle
            ]
            [ TypedSvg.Core.text <| String.fromFloat value ]
        ]
