module Main exposing (..)

import Basics exposing (pi)
import Html
import Json.Decode as Json
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseDown, onMouseUp)
import Task
import VirtualDom
import Window


type alias Position =
    { x : Int, y : Int }


type alias CircleData =
    ( Float, Float, Float, Int )


type alias Model =
    { size : Window.Size
    , pos : Position
    , mouseDown : Bool
    , circleData : List CircleData
    , currentCircleInd : Maybe Int
    }


type Msg
    = Error
    | WindowSize Window.Size
    | MouseMove Position
    | MouseDown Int
    | MouseUp


marginScene =
    20


test =
    "test2"


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { size = Window.Size 600 600
      , pos = Position 0 0
      , mouseDown = False
      , circleData =
            [ ( 0, 0, 16, 0 )
            , ( -160, 0, 12, 1 )
            , ( -112, 188, 24, 2 )
            , ( -50, 188, 20, 3 )
            ]
      , currentCircleInd = Nothing
      }
    , Task.perform WindowSize Window.size
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg |> Debug.log "msg" of
        WindowSize { width, height } ->
            ( { model | size = Window.Size (width - 2 * marginScene) (height - 100 - 2 * marginScene) }, Cmd.none )

        MouseMove pos ->
            if model.mouseDown then
                case model.currentCircleInd of
                    Nothing ->
                        ( { model | pos = pos }, Cmd.none )

                    Just ind ->
                        let
                            newCircleData =
                                updateCircleData model.circleData model.pos ind
                        in
                        ( { model | pos = pos, circleData = newCircleData }, Cmd.none )
            else
                ( model, Cmd.none )

        MouseDown ind ->
            ( { model
                | mouseDown = True
                , currentCircleInd = Just ind
              }
            , Cmd.none
            )

        MouseUp ->
            ( { model | mouseDown = False }, Cmd.none )

        _ ->
            Debug.crash "update"


updateCircleData : List CircleData -> Position -> Int -> List CircleData
updateCircleData list { x, y } ind =
    let
        toggle index ( ox, oy, rad, i ) =
            if i == ind then
                ( toFloat x - dispx, toFloat y - dispy, rad, i )
            else
                ( ox, oy, rad, i )
    in
    List.indexedMap toggle list


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text (toString model) ]
        , scene model
        ]


dispx : Float
dispx =
    200


dispy : Float
dispy =
    100


scene : Model -> Html.Html Msg
scene model =
    Svg.svg
        [ width <| toString model.size.width
        , height <| toString model.size.height
        , style ("margin-left:" ++ px marginScene)
        , VirtualDom.onWithOptions "mousemove" options (Json.map MouseMove offsetPosition)
        ]
        [ background model
        , grouping [ sproks model ]
        ]


px : a -> String
px n =
    toString n ++ "px"


background : Model -> Svg.Svg Msg
background model =
    Svg.rect
        [ width <| toString <| model.size.width - 20
        , height <| toString <| model.size.height - 20
        , fill "gray"
        ]
        []


{-| These options are an attempt to prevent double- and triple-clicking from
propagating and selecting text outside the SVG scene. Doesn't work.
-}
options =
    { preventDefault = True, stopPropagation = True }


offsetPosition : Json.Decoder Position
offsetPosition =
    Json.map2 Position (Json.field "offsetX" Json.int) (Json.field "offsetY" Json.int)


tracker : Model -> Svg Msg
tracker model =
    Svg.line
        [ x1 "0"
        , y1 "0"
        , x2 (toString model.pos.x)
        , y2 (toString model.pos.y)
        , style "stroke:rgb(255,0,0);stroke-width:2"
        ]
        []


circle : Model -> Svg Msg
circle model =
    Svg.circle
        [ r "20"
        , cx (toString model.pos.x)
        , cy (toString model.pos.y)
        ]
        []


grouping : List (Svg Msg) -> Svg Msg
grouping =
    Svg.g
        []


sproks : Model -> Svg Msg
sproks model =
    Svg.g []
        (List.map
            sprok
            model.circleData
        )


sprok : CircleData -> Svg Msg
sprok ( x, y, rad, ind ) =
    Svg.circle
        [ fill "blue"
        , r (toString rad)
        , cx (dispx + x |> toString)
        , cy (dispy + y |> toString)
        , strokeDasharray (toString (pi * 2))
        , strokeDashoffset "1"
        , stroke "#ff0"
        , strokeWidth "4"
        , onMouseDown (MouseDown ind)
        , onMouseUp MouseUp
        ]
        [ Svg.animate
            [ attributeName "stroke-dashoffset"
            , from "0"
            , to "6"
            , dur "1s"
            , repeatCount "indefinite"
            ]
            []
        ]


subscriptions model =
    Window.resizes WindowSize
