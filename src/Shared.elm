module Shared exposing (..)

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
import Color
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import List.Extra as List
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Quantity
import Svg exposing (Svg)
import Vector2d

-- Coordinate systems --

-- Geometry types with the units and coords type parameters applied --
type alias Axis = Axis2d Meters SceneCoords
type alias Circle = Circle2d Meters SceneCoords
type alias Direction = Direction2d SceneCoords
type alias LineSegment = LineSegment2d Meters SceneCoords
type alias Point = Point2d Meters SceneCoords
type alias Polygon = Polygon2d Meters SceneCoords
type alias Polyline = Polyline2d Meters SceneCoords


{- origin point in the center of the frame, x -> right, y -> up -}
type SceneCoords = SceneCoords

{- origin point at the top left of the frame, x -> right, y -> down -}
type TopLeftCoords = TopLeftCoords 

pixelsPerMeter = 
    pixels 100 
        |> Quantity.per (Length.meters 1)

-- Frames --

topLeftFrame : Frame2d Pixels SceneCoords { defines : TopLeftCoords }
topLeftFrame = 
    Frame2d.atOrigin
        |> Frame2d.translateBy
            (Vector2d.xy
                (pixels <| -constants.containerWidth / 2.0)
                (pixels <| constants.containerHeight / 2.0))
        |> Frame2d.reverseY

viewerFrame : Point -> Angle -> Frame2d Meters SceneCoords { defines : SceneCoords }
viewerFrame viewerPos viewerAngle = 
    roomFrame
        |> Frame2d.rotateAround Point2d.origin viewerAngle
        |> Frame2d.translateBy (Vector2d.from Point2d.origin viewerPos)
        |> Frame2d.relativeTo roomFrame

-- TODO delete this?
roomFrame : Frame2d Meters SceneCoords { defines : SceneCoords }
roomFrame = 
    Frame2d.atOrigin
    -- viewerFrame model
        -- |> Frame2d.translateBy (Vector2d.reverse 
        --     (Vector2d.from Point2d.origin model.viewerPos))
        -- |> Frame2d.rotateAround (Frame2d.originPoint <| viewerFrame model) 
        --     (Quantity.negate model.viewerAngle)


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


type alias SuccessAnimation = 
    { step : Int 
    , transitionPct : Maybe Float
    }


colors = 
    { blue1 = "#2c6fef"
    , yellow1 = "#eea71f"
    , red1 = "#e1503c"
    , green1 = "#179e7e"
    , darkBackground = Color.rgb 0.6 0.6 0.6
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
            (Frame2d.yDirection (viewerFrame viewerPos 
                (Direction2d.toAngle viewerDirection))))