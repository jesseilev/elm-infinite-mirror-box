module Convert exposing (..)

import Angle
import Axis2d exposing (Axis2d)
import Circle2d
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Html exposing (Html)
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity)
import Vector2d exposing (Vector2d)

pixelsPerMeter : Float 
pixelsPerMeter = 100

pixelsPerMeterRate : Quantity Float (Quantity.Rate Pixels Meters)
pixelsPerMeterRate = 
    Pixels.pixels pixelsPerMeter |> Quantity.per (Length.meters 1)

lengthToPixels : Length -> Quantity Float Pixels
lengthToPixels = 
    Quantity.at pixelsPerMeterRate

pixelsToLength : Quantity Float Pixels -> Length
pixelsToLength = 
    Quantity.at_ pixelsPerMeterRate

pointToPixels : Point2d Meters c -> Point2d Pixels c
pointToPixels p = 
    Point2d.xy 
        (Point2d.xCoordinate p |> lengthToPixels)
        (Point2d.yCoordinate p |> lengthToPixels)


pointToMeters : Point2d Pixels c -> Point2d Meters c
pointToMeters p = 
    Point2d.xy 
        (Point2d.xCoordinate p |> pixelsToLength)
        (Point2d.yCoordinate p |> pixelsToLength)

lineSegmentToPixels : LineSegment2d Meters c -> LineSegment2d Pixels c
lineSegmentToPixels ls = 
    LineSegment2d.from
        (LineSegment2d.startPoint ls |> pointToPixels)
        (LineSegment2d.endPoint ls |> pointToPixels)

polygonToPixels : Polygon2d Meters c -> Polygon2d Pixels c
polygonToPixels p = 
    Polygon2d.vertices p 
        |> List.map pointToPixels
        |> Polygon2d.singleLoop


polylineToPixels : Polyline2d Meters c -> Polyline2d Pixels c
polylineToPixels p = 
    Polyline2d.segments p
        |> List.map (LineSegment2d.endPoint >> pointToPixels)
        |> Polyline2d.fromVertices