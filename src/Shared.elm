module Shared exposing (..)

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
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
type alias LineSegment = LineSegment2d Meters SceneCoords
type alias Point = Point2d Meters SceneCoords
type alias Polygon = Polygon2d Meters SceneCoords
type alias Polyline = Polyline2d Meters SceneCoords


{- origin point in the center of the frame, x -> right, y -> up -}
type SceneCoords = SceneCoords

{- origin point at the top left of the frame, x -> right, y -> down -}
type TopLeftCoords = TopLeftCoords 

pixelsPerMeter zoomScale = 
    pixels 100 
        |> Quantity.per (Length.meters 1)
        |> Quantity.multiplyBy zoomScale

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