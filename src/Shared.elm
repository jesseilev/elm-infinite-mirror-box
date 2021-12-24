module Shared exposing (..)

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Length exposing (Meters)
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Quantity
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
                |> Quantity.multiplyBy pct
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
    { blue1 = "#88aaee"}


debugLogF : (a -> b) -> String -> a -> a
debugLogF f str a =
    let _ = Debug.log str (f a) in 
    a
