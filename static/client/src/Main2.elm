module Main2 exposing (main)


import Axis
import DateFormat
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import Time
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fill, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y, strokeWidth)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..), Paint(..))
import Json.Decode as Decode
import Json.Decode.Extra as JDE
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events
import Http
import List
import String
import Color
import Path exposing (Path)
import Shape


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
                    [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
                        [ xAxis (convert data) ]
                    , g [ transform [ Translate (padding - 1) padding ] ]
                        [ yAxis (convert data) ]
                    , g [ transform [ Translate padding padding ], class [ "series" ] ]
                        [ Path.element (area (convert data)) [ strokeWidth 3, fill <| Paint <| Color.white ]
                        , Path.element (line (convert data)) [ stroke <| Paint <| Color.rgb 0.25 0.41 0.88 , strokeWidth 2, fill PaintNone ]
                        ]
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

maximumFloat : List ( Time.Posix, Float ) -> Float
maximumFloat list =
    case list of
        [] -> 0
        [(_, y)] -> y
        ((_, y)::xs) ->
            let 
                maxTail = maximumFloat xs
            in
                if y > maxTail then
                    y
                else
                    maxTail


minimumFloat : List ( Time.Posix, Float ) -> Float
minimumFloat list =
    case list of
        [] -> 0
        [(_, y)] -> y
        ((_, y)::xs) ->
            let 
                minTail = minimumFloat xs
            in
                if y < minTail then
                    y
                else
                    minTail


maximumTime : List ( Time.Posix, Float ) -> Time.Posix
maximumTime list =
    case list of
        [] -> Time.millisToPosix 0
        [(x, _)] -> x
        ((x, _)::xs) ->
            let 
                maxTail = maximumTime xs
                valueMillis = Time.posixToMillis x
                maxTailMillis = Time.posixToMillis maxTail
            in
                if valueMillis > maxTailMillis then
                    x
                else
                    maxTail


minimumTime : List ( Time.Posix, Float ) -> Time.Posix
minimumTime list =
    case list of
        [] -> Time.millisToPosix 0
        [(x, _)] -> x
        ((x, _)::xs) ->
            let 
                minTail = minimumTime xs
                valueMillis = Time.posixToMillis x
                minTailMillis = Time.posixToMillis minTail
            in
                if valueMillis < minTailMillis then
                    x
                else
                    minTail


getFloatFromList : List ( Time.Posix, Float ) -> List Float
getFloatFromList list = 
    case list of
       [] -> []
       ((_, y)::xs) -> (y :: getFloatFromList xs) 


average : List ( Time.Posix, Float ) -> Basics.Float
average list =
    let
        listFl = getFloatFromList list
    in
        List.sum listFl / Basics.toFloat (List.length listFl)

xScale : List ( Time.Posix, Float ) -> ContinuousScale Time.Posix
xScale model =
    let 
        max = maximumTime model
        min = minimumTime model
    in
        Scale.time Time.utc ( 0, w - 2* padding ) ( min, max )


yScale : List ( Time.Posix, Float ) -> ContinuousScale Float
yScale model =
    let 
        max = maximumFloat model
        min = minimumFloat model
    in
        Scale.linear ( h - 2 * padding, 0 ) ( min, max )


dateFormat : Time.Posix -> String
dateFormat =
    DateFormat.format [ DateFormat.dayOfMonthFixed, DateFormat.text " ", DateFormat.monthNameAbbreviated, 
                        DateFormat.text " - ", DateFormat.hourMilitaryFixed, DateFormat.text ":", DateFormat.minuteFixed] Time.utc


xAxis : List ( Time.Posix, Float ) -> Svg msg
xAxis model =
    Axis.bottom [ Axis.tickCount (List.length model) ] (xScale model)


yAxis : List ( Time.Posix, Float ) -> Svg msg
yAxis model =
    Axis.left [ Axis.tickCount 10 ] (yScale model)

transformToLineData : List ( Time.Posix, Float ) -> ( Time.Posix, Float ) -> Maybe ( Float, Float )
transformToLineData model ( x, y ) =
    Just ( Scale.convert (xScale model) x, Scale.convert (yScale model) y )


tranfromToAreaData : List ( Time.Posix, Float ) -> ( Time.Posix, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
tranfromToAreaData model ( x, y ) =
    Just
        ( ( Scale.convert (xScale model) x, Tuple.first (Scale.rangeExtent (yScale model)) )
        , ( Scale.convert (xScale model) x, Scale.convert (yScale model) y )
        )


line : List ( Time.Posix, Float ) -> Path
line model =
    List.map (transformToLineData model) model
        |> Shape.line Shape.monotoneInXCurve


area : List ( Time.Posix, Float ) -> Path
area model =
    List.map (tranfromToAreaData model) model
        |> Shape.area Shape.monotoneInXCurve
