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
    { cx : Float
    , cy : Float
    , rad : Float
    , index : Int
    , segments : List Segment
    , side : Float
    , nowSegs : ( Int, Int )
    , prevSegs : ( Int, Int )
    , oneSeg : Int
    }


type alias Point =
    ( Float, Float )


type alias Segment =
    ( Point, Point )


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
    | RecaculatePath


marginScene =
    20


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


getCircleData : Float -> Float -> Float -> Int -> CircleData
getCircleData cx cy rad index =
    CircleData cx cy rad index [] 0.0 ( 0, 0 ) ( 0, 0 ) 0


init : ( Model, Cmd Msg )
init =
    ( { size = Window.Size 600 600
      , pos = Position 0 0
      , mouseDown = False
      , circleData =
            [ getCircleData 0 0 16 0
            , getCircleData -160 0 12 1
            , getCircleData -112 188 24 2
            , getCircleData -50 188 20 3
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
        toggle index c =
            if c.index == ind then
                getCircleData (toFloat x - dispx) (toFloat y - dispy) c.rad c.index
            else
                getCircleData c.cx c.cy c.rad c.index

        circlesWithUpdatePosition =
            List.indexedMap toggle list

        triGroup =
            tri circlesWithUpdatePosition

        -- find a way to get the previous element and the next element
        updatedCircles =
            List.map setupCircle triGroup
    in
    updatedCircles


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
        , grouping [ sproks model, mainPath model ]
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


mainPath : Model -> Svg msg
mainPath model =
    Svg.g [] []


setupCircle : ( CircleData, CircleData, CircleData ) -> CircleData
setupCircle ( cprev, cnow, cnext ) =
    let
        a =
            cnow.cx

        b =
            cnow.cy

        c =
            cnext.cx

        d =
            cnext.cy

        r0 =
            cnow.rad

        r1 =
            cnext.rad

        xp_out =
            (c * r0 - a * r1) / (r0 - r1)

        yp_out =
            (d * r0 - b * r1) / (r0 - r1)

        xp_in =
            (c * r0 + a * r1) / (r0 + r1)

        yp_in =
            (d * r0 + b * r1) / (r0 + r1)

        -- Outter tangent points
        ( ( xt1out, yt1out ), ( xt2out, yt2out ) ) =
            tangCi ( a, b ) r0 ( xp_out, yp_out )

        ( ( xt3out, yt3out ), ( xt4out, yt4out ) ) =
            tangCi ( c, d ) r1 ( xp_out, yp_out )

        -- Inner tangent points
        ( ( xt1in, yt1in ), ( xt2in, yt2in ) ) =
            tangCi ( a, b ) r0 ( xp_in, yp_in )

        ( ( xt3in, yt3in ), ( xt4in, yt4in ) ) =
            tangCi ( c, d ) r1 ( xp_in, yp_in )

        -- The 1,2 are the outter tangents
        -- the 3,4 are the inner tangents
        seg0 =
            ( ( xt1out, yt1out ), ( xt3out, yt3out ) )

        seg1 =
            ( ( xt2out, yt2out ), ( xt4out, yt4out ) )

        seg2 =
            ( ( xt1in, yt1in ), ( xt3in, yt3in ) )

        seg3 =
            ( ( xt2in, yt2in ), ( xt4in, yt4in ) )

        segments =
            if r0 < r1 then
                [ seg1, seg0, seg2, seg3 ]
            else
                [ seg0, seg1, seg2, seg3 ]

        side =
            sideOfLine ( cprev.cx, cprev.cy ) ( cnext.cx, cnext.cy ) ( cnow.cx, cnow.cy )
    in
    { cnow | segments = segments, side = side }


tangCi : Point -> Float -> Point -> ( Point, Point )
tangCi ( x, y ) r ( xp, yp ) =
    let
        xpx =
            xp - x

        ypy =
            yp - y

        xt sign =
            (r ^ 2 * xpx + sign * r * ypy * sqrt (xpx ^ 2 + ypy ^ 2 - r ^ 2))
                / (xpx ^ 2 + ypy ^ 2)
                + x

        yt sign =
            (r ^ 2 * ypy + sign * r * xpx * sqrt (ypy ^ 2 + xpx ^ 2 - r ^ 2))
                / (ypy ^ 2 + xpx ^ 2)
                + y
    in
    ( ( xt 1, yt -1 ), ( xt -1, yt 1 ) )


sideOfLine : Point -> Point -> Point -> Float
sideOfLine ( x1, y1 ) ( x2, y2 ) ( x, y ) =
    (x - x1) * (y2 - y1) - (y - y1) * (x2 - x1)


equal : Float -> Float -> Bool
equal a b =
    let
        eps =
            0.0001
    in
    abs (a - b) < eps


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
sprok c =
    Svg.circle
        [ fill "blue"
        , r (toString c.rad)
        , cx (dispx + c.cx |> toString)
        , cy (dispy + c.cy |> toString)
        , strokeDasharray (toString (pi * 2))
        , strokeDashoffset "1"
        , stroke "#ff0"
        , strokeWidth "4"
        , onMouseDown (MouseDown c.index)
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


tri : List a -> List ( a, a, a )
tri list =
    List.map3 tuple3 list (rollOnce list) (rollTwice list)


tuple3 : a -> b -> c -> ( a, b, c )
tuple3 a b c =
    ( a, b, c )


rollOnce : List a -> List a
rollOnce list =
    case list of
        [] ->
            []

        x :: xs ->
            xs ++ [ x ]


rollTwice : List a -> List a
rollTwice l =
    rollOnce (rollOnce l)
