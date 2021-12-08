module Playground1 exposing (..)

import Angle
import Axis2d exposing (Axis2d)
import Browser
import Circle2d
import Direction2d exposing (Direction2d)
import Element as El exposing (Element)
import Element.Border as Border
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Length exposing (Length)
import LineSegment2d exposing (LineSegment2d)
import Maybe.Extra as Maybe
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Vector2d exposing (Vector2d)

main = 
    Browser.element
        { init = init
        , update = update   
        , subscriptions = subscriptions
        , view = view
        }
        


type TopLeftCoords = TopLeftCoords 
type alias Point = Point2d Length TopLeftCoords

type alias Model = 
    { roomShape : Polygon2d Pixels TopLeftCoords
    , catFrame : Frame2d Pixels TopLeftCoords {}
    , targetDistance : Quantity Float Pixels
    }

init : () -> ( Model, Cmd Msg )
init _ =
    { roomShape = 
        Polygon2d.singleLoop 
            [ Point2d.pixels 50 50 
            , Point2d.pixels 350 70
            , Point2d.pixels 425 300
            , Point2d.pixels 100 450
            ]
    , catFrame = 
        Frame2d.atOrigin
            |> Frame2d.translateBy (Vector2d.pixels 90 150)
            |> Frame2d.rotateBy (Angle.degrees 315)
    , targetDistance = Pixels.float 2000
    }
        |> noCmds

        
type Msg 
    = NoOp
    | MouseDownAt (Float, Float)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        MouseDownAt (x, y) ->
            { model | catFrame = 
                model.catFrame |> pointXAxisAt (Point2d.pixels x y)
            }
                |> noCmds

        _ ->
            model |> noCmds


noCmds x = ( x, Cmd.none )


subscriptions _ = 
    Sub.none
        
constants =
    { }

view : Model -> Html Msg
view model = 
    El.layout 
        [ El.width El.fill
        , El.height El.fill 
        ]
        (El.el 
            [ El.centerX
            , El.centerY 
            , Border.solid
            , Border.width 2
            ] 
            (El.html (svgContainer model)))


svgContainer model =
    Svg.svg 
        [ Attr.width "500"
        , Attr.height "500"
        , Mouse.onDown (\event -> MouseDownAt event.offsetPos)
        ]
        [ diagram model ]

diagram : Model -> Svg Msg
diagram model =
    let 
        catLocation = 
            Frame2d.originPoint model.catFrame

        mirrorLine =
            LineSegment2d.from 
                (Point2d.pixels 0 100)
                (Point2d.pixels 300 100)

        mirrorLine2 = 
            LineSegment2d.from 
                (Point2d.pixels 0 300)
                (Point2d.pixels 300 300)

        sightLine = 
            LineSegment2d.fromPointAndVector catLocation
                (Vector2d.withLength model.targetDistance (Frame2d.xDirection model.catFrame))

        bouncePoint = 
            model.roomShape
                |> Polygon2d.edges
                |> List.map (LineSegment2d.intersectionPoint sightLine)
                |> Maybe.orList

        drawWall = 
            Svg.lineSegment2d
                [ Attr.stroke "grey" 
                , Attr.width "2"
                ]
    in
    Svg.g 
        []
        [ Svg.lineSegment2d 
            [ Attr.stroke "blue" 
            , Attr.width "2"
            ]
            sightLine
        , Svg.polygon2d 
            [ Attr.stroke "orange"
            , Attr.strokeWidth "4"
            , Attr.fill "none"
            ]
            model.roomShape
        , bouncePoint
            |> Maybe.map (\p -> 
                Svg.circle2d [ Attr.fill "cyan" ]
                    (Circle2d.atPoint p (Pixels.float 5))
            )
            |> Maybe.withDefault svgEmtpy
        , Svg.circle2d [ Attr.fill "purple" ]
            (Circle2d.atPoint catLocation (Pixels.float 4))
        , Svg.polyline2d
            [ Attr.stroke "cyan"
            , Attr.strokeWidth "6px"
            , Attr.fill "none"
            ]
            (bouncePath model.roomShape model.targetDistance model.catFrame)
        ]

svgEmtpy = Svg.g [] []


pointXAxisAt : Point2d Pixels TopLeftCoords -> Frame2d Pixels TopLeftCoords {} -> Frame2d Pixels TopLeftCoords {}
pointXAxisAt target frame =
    let 
        currentDirection =
            Frame2d.xDirection frame

        targetDirection = 
            Vector2d.from (Frame2d.originPoint frame) target
                |> Vector2d.direction
                |> Maybe.withDefault currentDirection

        angleDiff = 
            Direction2d.angleFrom currentDirection targetDirection
    in 
        Frame2d.rotateBy angleDiff frame

bouncePath : Polygon2d u c -> Quantity Float u -> Frame2d u c d -> Polyline2d u c
bouncePath wallShape gazeDistance frame = 
    let 
        position = 
            Frame2d.originPoint frame

        gazeVector = 
            Vector2d.withLength gazeDistance (Frame2d.xDirection frame)

        endPoint =
            Point2d.translateBy gazeVector position

        gazePathStraight = 
            LineSegment2d.from position endPoint

        bounceM : Maybe { point : Point2d u c, wall: LineSegment2d u c }
        bounceM = 
            wallShape
                |> Polygon2d.edges
                |> List.map (\e -> 
                    LineSegment2d.intersectionPoint gazePathStraight e 
                        |> Maybe.map (\p -> { point = p, wall = e }))
                |> Maybe.orList

        -- gazePathTail =
        --     bounceM
        --         |> Maybe.andThen (\b -> LineSegment2d.direction b.wall |> Maybe.map (\dir -> (b, dir)))
        --         |> Maybe.map (\(b, wallDirection) -> 
        --             LineSegment2d.from b.point endPoint
        --                 |> LineSegment2d.mirrorAcross (Axis2d.through b.point wallDirection))
        
        recurse bounce = 
            let 
                newDistance = 
                    gazeDistance |> Quantity.minus (Point2d.distanceFrom position bounce.point)

                wallDirection = 
                    LineSegment2d.direction bounce.wall 
                        |> Maybe.withDefault Direction2d.x

                newDirection = 
                    Frame2d.xDirection frame
                        |> Direction2d.mirrorAcross (Axis2d.through bounce.point wallDirection)

                newFrame = 
                    Frame2d.withXDirection newDirection bounce.point
            in 
                bouncePath wallShape newDistance newFrame

        vertices = 
            bounceM 
                |> Maybe.map (\b -> b.point :: (Polyline2d.vertices (recurse b)))
                |> Maybe.withDefault [ endPoint ]
    in
        Polyline2d.fromVertices vertices


maybePair : Maybe a -> Maybe b -> Maybe (a, b)
maybePair ma mb = 
    case (ma, mb) of 
        (Just x, Just y) -> Just (x, y)
        _ -> Nothing