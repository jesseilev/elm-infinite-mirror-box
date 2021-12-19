module Types exposing (..)

import Axis2d exposing (Axis2d)
import Circle2d exposing (Circle2d)
import Length exposing (Meters)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)


-- Coordinate systems --

{- origin point in the center of the frame, x -> right, y -> up -}
type SceneCoords = SceneCoords

{- origin point at the top left of the frame, x -> right, y -> down -}
type TopLeftCoords = TopLeftCoords 


-- Geometry types with the units and coords type parameters applied --
type alias Point = Point2d Meters SceneCoords
type alias LineSegment = LineSegment2d Meters SceneCoords
type alias Circle = Circle2d Meters SceneCoords
type alias Polygon = Polygon2d Meters SceneCoords
type alias Polyline = Polyline2d Meters SceneCoords
type alias Axis = Axis2d Meters SceneCoords
