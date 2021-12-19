module Room exposing (Model, init1, Msg, update, mouseDragMsg, view)

import Array
import Angle exposing (Angle)
import Axis2d
import Circle2d
import Color
import Direction2d
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import LineSegment2d
import List.Nonempty
import Length exposing (Meters)
import Maybe.Extra as Maybe
import Quantity exposing (Quantity)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Polyline2d
import Polygon2d
import RoomItem
import Sightray exposing (Sightray)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import TypedSvg.Attributes
import TypedSvg.Types exposing (CoordinateSystem(..), Paint(..))
import Shared exposing
    ( Axis
    , LineSegment
    , Point 
    , Polygon 
    , SceneCoords
    , TopLeftCoords
    )
import Vector2d

type alias Hallway =
    { rooms : NonemptyList Room }

type alias Room = 
    { roomStuff : () 
    , sightray : Sightray
    }

type alias NonemptyList a = List.Nonempty.Nonempty a


type alias Model = 
    { wallShape : Polygon
    , viewerPos : Point
    , viewerAngle : Angle
    , sightDistance : Quantity Float Meters
    , targetPos : Point
    }

init1 : Model 
init1 =
    { wallShape = 
        Polygon2d.singleLoop 
            [ Point2d.meters -2.0 -2.0
            , Point2d.meters 2.0 -1.0
            , Point2d.meters 2.25 1.0
            , Point2d.meters -1.5 2.25
            ]
    , viewerPos = Point2d.meters 0.3 -0.7
    , viewerAngle = Angle.degrees 50
    , sightDistance = Length.meters 14.5
    , targetPos = Point2d.meters 1.5 0.2
    }

-- FUNCTIONS


-- nextMirrorBounce : LineSegment -> Polygon -> Maybe MirrorBounce
-- nextMirrorBounce sightline roomShape =
--     let
--         -- "trim" off the very beginning of the sightline by shrinking it slightly
--         -- we dont want to count the start point as an intersection
--         -- which will happen for all recursive calls since the sightline will start on a wall
--         trimmedSightline = 
--             LineSegment2d.scaleAbout (LineSegment2d.endPoint sightline) 0.999 sightline    
--     in
--     Polygon2d.edges roomShape
--         |> List.map (\e -> 
--             (LineSegment2d.intersectionPoint trimmedSightline e
--                 |> Maybe.map (Tuple.pair e)))
--         |> Maybe.orList
--         |> Maybe.andThen (\(e, p) -> LineSegment2d.direction e |> Maybe.map (\d -> (e, p, d)))
--         |> Maybe.map (\(wall, point, dir) -> 
--             { wall = wall, point = point, axis = Axis2d.withDirection dir point })

-- -- TODO remake this using Room type
-- reflectedRooms : LineSegment -> Polygon -> List Polygon -> List Polygon
-- reflectedRooms sightline room roomsAcc = 
--     nextMirrorBounce sightline room
--         |> Maybe.map (\inter -> 
--             reflectedRooms 
--                 (LineSegment2d.from inter.point (LineSegment2d.endPoint sightline))
--                 (room |> Polygon2d.mirrorAcross inter.axis)
--                 (room :: roomsAcc))
--         |> Maybe.withDefault (room :: roomsAcc)

projectedSightline : Model -> LineSegment
projectedSightline model =
    LineSegment2d.fromPointAndVector model.viewerPos
        (Vector2d.withLength model.sightDistance 
            (Frame2d.yDirection (Shared.viewerFrame model.viewerPos model.viewerAngle)))


angleDiff : Point2d u c -> Point2d u c -> Point2d u c -> Maybe Angle
angleDiff pivot p1 p2 = 
    let
        getAngle p = 
            Direction2d.from pivot p 
                |> Maybe.map Direction2d.toAngle
    in
        Maybe.map2 Quantity.difference (getAngle p2) (getAngle p1)


-- UPDATE

type Msg 
    = NoOp
    | MouseDragMsg Point Point
    | RoomItemMsg RoomItem.Msg

mouseDragMsg : Point -> Point -> Msg
mouseDragMsg = 
    MouseDragMsg

update : Msg -> Model -> Model
update msg model =
    case msg of 
        MouseDragMsg prevMousePos mousePos -> 
            Shared.viewerFrame model.viewerPos model.viewerAngle
                |> Frame2d.originPoint
                |> (\origin -> angleDiff origin prevMousePos mousePos)
                |> Maybe.map (Quantity.plus model.viewerAngle)
                |> Maybe.withDefault model.viewerAngle
                |> (\a -> { model | viewerAngle = a })
        _ -> 
            model

-- VIEW

view : Maybe Int -> Model -> Svg Msg 
view animationStage model = 
    let 
        viewRoomItem point emoji = 
            RoomItem.init point emoji
                |> RoomItem.view
                |> Svg.map RoomItemMsg
    in
    Svg.g [] 
        [ Svg.polygon2d 
            [ Attr.fill "none"
            , Attr.strokeWidth "0.02"
            , Attr.stroke "black"
            ]
            (model.wallShape |> Polygon2d.placeIn Shared.roomFrame)
        -- , viewReflectedRooms model
        , Sightray.fromRoomAndProjectedPath model.wallShape (projectedSightline model)
            |> (\ray -> 
                animationStage 
                    |> Maybe.map (viewRayUnfoldAnimation model ray)
                    |> Maybe.withDefault (viewRay model.viewerPos ray))
        , viewRoomItem (Point2d.meters -0.5 0.3) RoomItem.emojis.roundTree
        , viewRoomItem (Point2d.meters 0.2 0.9) RoomItem.emojis.roundTree
        , viewRoomItem (Point2d.meters 1.1 -0.4) RoomItem.emojis.pineTree
        , viewRoomItem model.viewerPos RoomItem.emojis.cat
        , viewRoomItem model.targetPos RoomItem.emojis.parrot
        -- , viewDebugStuff model
        ]
        |> Svg.at (Shared.pixelsPerMeter 0.7)
        |> Svg.relativeTo Shared.topLeftFrame

viewRay : Point -> Sightray -> Svg Msg 
viewRay viewerPos rp = 
    rp
        |> Sightray.vertices
        |> (\vs -> viewerPos :: vs)
        |> Polyline2d.fromVertices
        |> Svg.polyline2d
            [ Attr.fill "none"
            , Attr.stroke "black"
            , Attr.strokeWidth "0.05"
            , Attr.strokeDasharray "0.05"
            ]

viewRayUnfoldAnimation : Model -> Sightray -> Int -> Svg Msg 
viewRayUnfoldAnimation model ray stage = 
    ray
        |> Sightray.unfold
        |> Array.fromList
        |> Array.get stage
        |> Maybe.map (viewRay model.viewerPos)
        |> Maybe.withDefault (Svg.g [] [])





-- viewReflectedRooms : Model -> Svg Msg 
-- viewReflectedRooms model = 
--     let
--         reflectedRoom mirrorWall = 
--             model.roomShape 
--                 |> Polygon2d.mirrorAcross mirrorWall.axis
        
--         wallM = 
--             Polygon2d.edges model.roomShape
--                 |> List.head
--                 |> Maybe.andThen mkWall

--         mkWall : LineSegment -> Maybe { wall : LineSegment, axis : Axis }
--         mkWall line = 
--             LineSegment2d.direction line
--                 |> Maybe.map (Axis2d.through (LineSegment2d.startPoint line))
--                 |> Maybe.map (\a -> { wall = line, axis = a})
--     in
--         reflectedRooms (projectedSightline model) model.roomShape []
--             |> List.map (Svg.polygon2d 
--                 [ Attr.fill "none"
--                 , Attr.stroke "grey"
--                 , Attr.strokeWidth "0.02" 
--                 ])
--             |> Svg.g [] 



-- viewDebugStuff : Model -> Svg Msg 
-- viewDebugStuff model = 
--     Svg.g []
--         [ frameDebugViz "purple"
--             |> Svg.placeIn (roomFrame model)
--         , frameDebugViz "orange" 
--             |> Svg.placeIn (viewerFrame model)
--         -- , frameDebugViz "grey"
--         --     |> Svg.relativeTo ((topLeftFrame model) |> Frame2d.translateBy (Vector2d.pixels -100 -100))
--         , Svg.circle2d
--             [ Attr.fill "red" ]
--             (Circle2d.atPoint model.clickPosDebug (Length.meters 0.05))
--         ]