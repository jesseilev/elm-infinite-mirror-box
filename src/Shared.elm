module Shared exposing (..)

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
import Color
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import List.Extra as List
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Vector2d exposing (Vector2d)
import Rectangle2d exposing (Rectangle2d)

-- Coordinate systems --

-- Geometry types with the units and coords type parameters applied --
type alias Arc = Arc2d Meters SceneCoords
type alias Axis = Axis2d Meters SceneCoords
type alias Circle = Circle2d Meters SceneCoords
type alias Direction = Direction2d SceneCoords
type alias LineSegment = LineSegment2d Meters SceneCoords
type alias Point = Point2d Meters SceneCoords
type alias Polygon = Polygon2d Meters SceneCoords
type alias Polyline = Polyline2d Meters SceneCoords
type alias Vector = Vector2d Meters SceneCoords


{- origin point in the center of the frame, x -> right, y -> up -}
type SceneCoords = SceneCoords

{- origin point at the top left of the frame, x -> right, y -> down -}
type SvgCoords = SvgCoords 

pixelsPerMeter = 
    pixels 100 
        |> Quantity.per (Length.meters 1)

-- Frames --

svgFrame : Frame2d Pixels SceneCoords { defines : SvgCoords }
svgFrame = 
    Frame2d.atOrigin
        |> Frame2d.translateBy
            (Vector2d.xy
                (pixels <| -constants.containerWidth / 2.0)
                (pixels <| constants.containerHeight / 2.0))
        |> Frame2d.reverseY
        

playerFrame : Point -> Angle -> Frame2d Meters SceneCoords { defines : SceneCoords }
playerFrame viewerPos viewerAngle = 
    roomFrame
        |> Frame2d.rotateAround Point2d.origin viewerAngle
        |> Frame2d.translateBy (Vector2d.from Point2d.origin viewerPos)
        |> Frame2d.relativeTo roomFrame

-- TODO delete this?
roomFrame : Frame2d Meters SceneCoords { defines : SceneCoords }
roomFrame = 
    Frame2d.atOrigin
    -- playerFrame model
        -- |> Frame2d.translateBy (Vector2d.reverse 
        --     (Vector2d.from Point2d.origin model.viewerPos))
        -- |> Frame2d.rotateAround (Frame2d.originPoint <| playerFrame model) 
        --     (Quantity.negate model.viewerAngle)

viewFrame : String -> Frame2d Meters SceneCoords { defines : SceneCoords } -> Svg msg
viewFrame color frame = 
    let originPoint = Frame2d.originPoint frame in
    Svg.g 
        []
        [ Svg.rectangle2d [ Attr.fill color, Attr.opacity "0.2" ]
            (Rectangle2d.from originPoint
                (originPoint
                    |> Point2d.translateIn (Frame2d.xDirection frame) (Length.meters 0.25)
                    |> Point2d.translateIn (Frame2d.yDirection frame) (Length.meters 0.95)
                )
            )
        ]


constants =
    { containerWidth = 500 
    , containerHeight = 500 
    }




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

type alias InterpolatedReflection a = Axis -> Float -> a -> a

interpReflectPoint : InterpolatedReflection Point 
interpReflectPoint axis pct point =
    Point2d.mirrorAcross axis point
        |> (LineSegment2d.from point)
        |> (\line -> LineSegment2d.interpolate line pct)

interpReflectLine : InterpolatedReflection LineSegment
interpReflectLine axis pct line =
    LineSegment2d.from
        (interpReflectPoint axis pct (LineSegment2d.startPoint line))
        (interpReflectPoint axis pct (LineSegment2d.endPoint line))

interpReflectPolygon : InterpolatedReflection Polygon 
interpReflectPolygon axis pct pg = 
    Polygon2d.vertices pg
        |> List.map (interpReflectPoint axis pct)
        |> Polygon2d.singleLoop

interpReflectPolyline : InterpolatedReflection Polyline
interpReflectPolyline axis pct pl =
    Polyline2d.vertices pl
        |> List.map (interpReflectPoint axis pct)
        |> Polyline2d.fromVertices


interpReflectDirection : Axis -> Float -> Direction -> Direction
interpReflectDirection axis pct dir = 
    let 
        angle = Direction2d.toAngle dir 

        interpAngle : Angle -> Angle -> Float -> Angle
        interpAngle a1 a2 pct_ = 
            Quantity.difference a1 a2
                |> Quantity.multiplyBy pct_
                |> Quantity.plus a1

        fullReflectionAngle = 
            Direction2d.mirrorAcross axis dir
                |> Direction2d.toAngle
    in
        interpAngle angle fullReflectionAngle pct
            |> Direction2d.fromAngle



colors = 
    { blue1 = "#2c6fef"
    , yellow1 = "#eea71f"
    , red1 = "#e1503c"
    , green1 = "#179e7e"
    , darkBackground = Color.rgb 0.6 0.6 0.6
    , roomBackground = "none"
    }


debugLogF : String -> (a -> b) -> a -> a
debugLogF str f a =
    let _ = Debug.log str (f a) in 
    a


angleDiff : Point2d u c -> Point2d u c -> Point2d u c -> Maybe Angle
angleDiff pivot p1 p2 = 
    let
        getAngle p = 
            Direction2d.from pivot p 
                |> Maybe.map Direction2d.toAngle
                -- |> debugLogF "two points" (\m -> (m, pivot, p))
    in
        Maybe.map2 Quantity.difference (getAngle p2) (getAngle p1)
            -- |> Debug.log "anglediff retval"


svgEmpty : Svg msg 
svgEmpty = 
    Svg.g [] []

segmentSamplePoints : LineSegment -> List Point
segmentSamplePoints line = 
    let sampleCount = 25 in
    List.range 0 sampleCount 
        |> List.map (\i -> 
            LineSegment2d.interpolate line (toFloat i / toFloat sampleCount)
        )

projectedSightline : Point -> Direction -> Length -> LineSegment
projectedSightline viewerPos viewerDirection sightDistance =
    LineSegment2d.fromPointAndVector viewerPos
        (Vector2d.withLength sightDistance 
            (Frame2d.xDirection (playerFrame viewerPos 
                (Direction2d.toAngle viewerDirection))))

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

takeMin : (a -> Quantity number c) -> a -> a -> a
takeMin quantify p q = 
    case Quantity.compare (quantify p) (quantify q) of
        GT -> q
        _ -> p

debugCircle : String -> Point -> Svg msg
debugCircle color pos = 
    Svg.circle2d [ Attr.fill color ]
        (Circle2d.atPoint pos (Length.meters 0.125))


floatAttribute zoomScale f = 
    f / zoomScale |> String.fromFloat
