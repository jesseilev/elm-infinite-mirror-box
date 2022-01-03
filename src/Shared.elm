module Shared exposing (..)

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
import Color
import Direction2d exposing (Direction2d)
import Element as El exposing (Element)
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import List.Extra as List
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Rectangle2d exposing (Rectangle2d)
import Triangle2d exposing (Triangle2d)
import Quantity exposing (Quantity)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Vector2d exposing (Vector2d)


-- Geometry types with the units and coordinates type parameters applied --
type alias Arc = Arc2d Meters SceneCoords
type alias Axis = Axis2d Meters SceneCoords
type alias Circle = Circle2d Meters SceneCoords
type alias Direction = Direction2d SceneCoords
type alias LineSegment = LineSegment2d Meters SceneCoords
type alias Point = Point2d Meters SceneCoords
type alias Polygon = Polygon2d Meters SceneCoords
type alias Polyline = Polyline2d Meters SceneCoords
type alias Triangle = Triangle2d Meters SceneCoords
type alias Vector = Vector2d Meters SceneCoords


{- origin point in the center of the frame, x -> right, y -> up -}
type SceneCoords = SceneCoords

{- origin point at the top left of the frame, x -> right, y -> down -}
type SvgCoords = SvgCoords 


-- Frames --

svgFrame : Frame2d Pixels SceneCoords { defines : SvgCoords }
svgFrame = 
    Frame2d.atOrigin
        |> Frame2d.translateBy
            (Vector2d.xy
                (pixels <| -svgWidth / 2.0)
                (pixels <| svgHeight / 2.0))
        |> Frame2d.reverseY

playerFrame : Point -> Angle -> Frame2d Meters SceneCoords { defines : SceneCoords }
playerFrame viewerPos viewerAngle = 
    roomFrame
        |> Frame2d.rotateAround Point2d.origin viewerAngle
        |> Frame2d.translateBy (Vector2d.from Point2d.origin viewerPos)
        |> Frame2d.relativeTo roomFrame

roomFrame : Frame2d Meters SceneCoords { defines : SceneCoords }
roomFrame = 
    Frame2d.atOrigin



-- Constants --
colors = 
    { yellow1 = "#eea71f"
    , yellowLight = "#f2b135"
    , yellowDark = "#df9912"
    , blue1 = "#2c6fef"
    , red1 = "#e1503c"
    , green1 = "#179e7e"
    , greyVeryLight = "#eee"
    , greyLight = "#ccc"
    , greyMedium = "#888"
    , greyDark = "#555"
    }

svgWidth = 500
svgHeight = 500 

pixelsPerMeter = 
    pixels 100
        |> Quantity.per (Length.meters 1)

-- Misc Util --

noCmds : a -> (a, Cmd msg)
noCmds x = ( x, Cmd.none )


maybePair : Maybe a -> Maybe b -> Maybe (a, b)
maybePair ma mb = 
    case (ma, mb) of 
        (Just x, Just y) -> Just (x, y)
        _ -> Nothing

sign : number -> number
sign n = 
    if n < 0 then -1 else 1

debugLogF : String -> (a -> b) -> a -> a
debugLogF str f a =
    let _ = Debug.log str (f a) in 
    a


takeMin : (a -> Quantity number c) -> a -> a -> a
takeMin quantify p q = 
    case Quantity.compare (quantify p) (quantify q) of
        GT -> q
        _ -> p



-- Geometry and Math --

angleDiff : Point2d u c -> Point2d u c -> Point2d u c -> Maybe Angle
angleDiff pivot p1 p2 = 
    let
        getAngle p = 
            Direction2d.from pivot p 
                |> Maybe.map Direction2d.toAngle
    in
        Maybe.map2 Quantity.difference (getAngle p2) (getAngle p1)

segmentSamplePoints : LineSegment -> List Point
segmentSamplePoints line = 
    let sampleCount = 25 in
    List.range 0 sampleCount 
        |> List.map (\i -> 
            LineSegment2d.interpolate line (toFloat i / toFloat sampleCount)
        )

{-
Normalizes an angle to the range of -180 thru 180
This ensures the angle doesn't go the "wrong way" around the circle, ie the long way around
normalizeAngle (Angle.degrees 181) == Angle.degrees -179
normalizeAngle (Angle.degrees -181) == Angle.degrees 179
TODO -- only works for inputs in the range -360 thru 360, could fix this with modular math
-}
normalizeAngle : Angle -> Angle 
normalizeAngle angle = 
    let 
        outOfRange = 
            (angle |> Quantity.greaterThan (Angle.degrees 180))
                || (angle |> Quantity.lessThan (Angle.degrees 180 |> Quantity.negate))

        normalizeTooBig a = 
            if a |> Quantity.greaterThan (Angle.degrees 180) then 
                a |> Quantity.minus (Angle.degrees 360)
            else 
                a

        normalizeTooNegative a = 
            if a |> Quantity.lessThan (Angle.degrees 180 |> Quantity.negate) then 
                a |> Quantity.plus (Angle.degrees 360)
            else 
                a
    in
        angle 
            |> normalizeTooBig
            |> normalizeTooNegative

triangleForArc : Arc -> Triangle
triangleForArc arc = 
    Triangle2d.from (Arc2d.centerPoint arc)
        (Arc2d.startPoint arc) 
        (Arc2d.endPoint arc)

projectedSightline : Point -> Direction -> Length -> LineSegment
projectedSightline viewerPos viewerDirection sightDistance =
    LineSegment2d.fromPointAndVector viewerPos
        (Vector2d.withLength sightDistance 
            (Frame2d.xDirection (playerFrame viewerPos 
                (Direction2d.toAngle viewerDirection))))


-- Interpolation --

type alias Interpolation a = a -> a -> Float -> a

interpolateLists : (Interpolation a) -> Interpolation (List a)
interpolateLists interpA l1 l2 pct =
    List.zip l1 l2 
        |> List.map (\(a1, a2) -> interpA a1 a2 pct)

interpolatePointFrom : Interpolation Point
interpolatePointFrom p1 p2 pct = 
    LineSegment2d.from p1 p2
        |> (\line -> LineSegment2d.interpolate line pct)

interpolateLineFrom : Interpolation LineSegment 
interpolateLineFrom l1 l2 pct = 
    LineSegment2d.from 
        (interpolatePointFrom (LineSegment2d.startPoint l1) (LineSegment2d.startPoint l2) pct)
        (interpolatePointFrom (LineSegment2d.endPoint l1) (LineSegment2d.endPoint l2) pct)

interpolatePolygonFrom : Interpolation Polygon
interpolatePolygonFrom p1 p2 pct =
    interpolateLists interpolatePointFrom (Polygon2d.vertices p1) (Polygon2d.vertices p2) pct
        |> Polygon2d.singleLoop

interpolateDirectionFrom : Interpolation Direction
interpolateDirectionFrom d1 d2 pct =
    interpolateAngleFrom (Direction2d.toAngle d1) (Direction2d.toAngle d2) pct
        |> Direction2d.fromAngle

interpolateAngleFrom : Interpolation Angle
interpolateAngleFrom a1 a2 pct = 
    Quantity.difference a1 a2
        |> Quantity.multiplyBy pct
        |> Quantity.plus a1

interpolateAxisFrom : Interpolation Axis 
interpolateAxisFrom a1 a2 pct = 
    Axis2d.through 
        (interpolatePointFrom (Axis2d.originPoint a1) (Axis2d.originPoint a2) pct)
        (interpolateDirectionFrom (Axis2d.direction a1) (Axis2d.direction a2) pct)


-- Svg --

floatAttributeForZoom : Float -> Float -> String
floatAttributeForZoom zoomScale f = 
    f / zoomScale |> String.fromFloat

svgEmpty : Svg msg 
svgEmpty = 
    Svg.g [] []

debugCircle : String -> Point -> Svg msg
debugCircle color pos = 
    Svg.circle2d [ Attr.fill color ]
        (Circle2d.atPoint pos (Length.meters 0.125))

svgLabel : String -> Point -> Vector -> String -> Svg msg
svgLabel color startPoint vector text = 
    let 
        endPoint = Point2d.translateBy vector startPoint 
        textPoint = Point2d.translateBy (Vector2d.scaleBy 1.5 vector) startPoint
        fontSize = 0.125
    in
    Svg.g []
        [ Svg.circle2d [ Attr.fill color ]
            (Circle2d.atPoint startPoint (Length.meters 0.02))
        , Svg.lineSegment2d 
            [ Attr.fill "none", Attr.stroke color, Attr.strokeWidth (String.fromFloat 0.01) ]
            (LineSegment2d.from startPoint endPoint)
        , Svg.text_ 
            [ Attr.fontSize <| String.fromFloat fontSize
            , Attr.x <| String.fromFloat (-0.5 * fontSize)
            , Attr.fill color
            , Attr.alignmentBaseline "central"
            ] 
            [ Svg.text text ]
            |> Svg.mirrorAcross (Axis2d.through Point2d.origin Direction2d.x)
            |> Svg.translateBy (Vector2d.from Point2d.origin textPoint)
        ]
