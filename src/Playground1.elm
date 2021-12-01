module Playground1 exposing (..)

import Angle
import Browser
import Circle2d
import Direction2d exposing (Direction2d)
import Element as El exposing (Element)
import Element.Border as Border
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
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
    , targetDistance = Pixels.float 300
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

-- bouncePath : Polygon2d -> Point2d -> Direction2d -> Polyline2d
-- bouncePath wallShape position gazeDirection = 
--     Polyline2d.fromVertices []