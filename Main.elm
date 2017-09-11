module Main exposing (..)

import Basics exposing (pi)
import Set
import Html
import Json.Decode as Json
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onMouseDown, onMouseUp)
import Svg.Path exposing (..)
import Task
import VirtualDom
import Window
import List.Extra exposing (..)
import Debug exposing (log)


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
    , seg : Segment
    , aside : Float
    , arcFlag : ArcFlag
    , direction : Direction
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
    CircleData cx cy rad index [] 0.0 ( 0, 0 ) ( 0, 0 ) 0 ( ( 0, 0 ), ( 0, 0 ) ) 0 largestArc clockwise


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
    case msg of
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

        triGroup2 =
            tri updatedCircles

        updatedCircles2 =
            List.map setupCircle2 triGroup2

        triGroup3 =
            tri updatedCircles2

        updatedCircles3 =
            List.map setupCircle3 triGroup3
    in
        updatedCircles3


view : Model -> Html.Html Msg
view model =
    let
        todisp =
            List.map (\c -> (c.oneSeg)) model.circleData
    in
        Html.div []
            [ scene model
            , Html.div [] [ Html.text (toString todisp) ]
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


cross =
    [ ( -60, -20 )
    , ( -20, -20 )
    , ( -20, -60 )
    , ( 20, -60 )
    , ( 20, -20 )
    , ( 60, -20 )
    , ( 60, 20 )
    , ( 20, 20 )
    , ( 20, 60 )
    , ( -20, 60 )
    , ( -20, 20 )
    , ( -60, 20 )
    ]


polygon ps =
    case ps of
        [] ->
            emptySubpath

        x :: xs ->
            subpath (startAt x) closed [ lineToMany xs ]


first : ( a, b ) -> a
first ( n, m ) =
    n


second ( n, m ) =
    m


mainPath : Model -> Svg msg
mainPath model =
    let
        triCircles =
            tri model.circleData

        prevs =
            List.map (\( p, _, _ ) -> p) triCircles

        cl =
            case List.head triCircles of
                Nothing ->
                    getCircleData 0 0 0 0

                Just ( p, _, _ ) ->
                    p

        segs =
            List.map (\c -> (second c.seg)) prevs

        arcs =
            List.map (\c -> arcTo ( c.rad, c.rad ) 0 ( c.arcFlag, c.direction ) (first c.seg)) model.circleData

        addDisp =
            (\( a, b ) -> ( a + dispx, b + dispy ))

        addedDisps =
            List.map addDisp segs

        f =
            subpath (startAt (addDisp (first cl.seg)))
                Svg.Path.open
                (List.map
                    lineTo
                    addedDisps
                )
    in
        Svg.g []
            [ Svg.path
                [ d (pathToString [ f ])
                , stroke "red"
                , fill "none"
                , strokeLinejoin "round"
                , strokeWidth "4"
                ]
                []
            ]


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

        nowSegs =
            if side > 0 then
                ( 0, 2 )
            else
                ( 1, 3 )

        prevSegs =
            if side > 0 then
                ( 0, 3 )
            else
                ( 1, 2 )
    in
        { cnow | segments = segments, side = side, nowSegs = nowSegs, prevSegs = prevSegs }


setFromTuple : ( comparable, comparable ) -> Set.Set comparable
setFromTuple ( n, m ) =
    Set.fromList [ n, m ]


setupCircle2 : ( CircleData, CircleData, CircleData ) -> CircleData
setupCircle2 ( cprev, cnow, cnext ) =
    let
        set1 =
            setFromTuple cnow.nowSegs

        set2 =
            setFromTuple cnext.prevSegs

        setUnion =
            Set.intersect set1 set2 |> Debug.log "union"

        first =
            List.head (Set.toList setUnion)

        oneSeg =
            case first of
                Nothing ->
                    -1

                Just n ->
                    n

        seg =
            case cnow.segments !! oneSeg of
                Nothing ->
                    ( ( 0, 0 ), ( 0, 0 ) )

                Just s ->
                    s
    in
        { cnow | seg = seg, oneSeg = oneSeg }


setupCircle3 : ( CircleData, CircleData, CircleData ) -> CircleData
setupCircle3 ( cprev, cnow, cnext ) =
    let
        aside =
            arcSide cprev.seg cnow.seg

        direction =
            if cnow.side > 0 then
                clockwise
            else
                antiClockwise

        arcFlag =
            if direction == clockwise then
                (if aside < 0 then
                    largestArc
                 else
                    smallestArc
                )
            else
                (if aside > 0 then
                    largestArc
                 else
                    smallestArc
                )
    in
        { cnow | aside = aside, direction = direction, arcFlag = arcFlag }


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


arcSide : Segment -> Segment -> Float
arcSide ( ( x0, y0 ), ( x1, y1 ) ) ( ( x2, y2 ), ( x3, y3 ) ) =
    let
        difx =
            x3 - x0

        dify =
            y3 - y0
    in
        sideOfLine ( x0, y0 ) ( x1, y1 ) ( x2 - difx, y2 - dify )


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


lastElem : List a -> Maybe a
lastElem list =
    case list of
        [] ->
            Nothing

        [ last ] ->
            Just last

        head :: rest ->
            lastElem rest


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
    List.map3 tuple3 (rollFront list) list (rollBack list)


tuple3 : a -> b -> c -> ( a, b, c )
tuple3 a b c =
    ( a, b, c )


rollBack : List a -> List a
rollBack list =
    case list of
        [] ->
            []

        x :: xs ->
            xs ++ [ x ]


rollFront : List a -> List a
rollFront list =
    reverse (rollBack (reverse list))


rollTwice : List a -> List a
rollTwice l =
    rollBack (rollBack l)



-------------------- Roll back


rollies : List a -> List a
rollies list =
    let
        lastOne =
            last list
    in
        case lastOne of
            Nothing ->
                []

            Just l ->
                l :: butLast list


normalTail : List a -> List a
normalTail list =
    let
        maybeTail =
            List.tail list
    in
        case maybeTail of
            Nothing ->
                []

            Just list ->
                list


butLast : List a -> List a
butLast list =
    if list == [] then
        []
    else
        reverse (normalTail (reverse list))


last : List a -> Maybe a
last list =
    if list == [] then
        Nothing
    else
        List.head (reverse list)


reverse : List a -> List a
reverse list =
    case list of
        [] ->
            []

        x :: xs ->
            reverse xs ++ [ x ]
