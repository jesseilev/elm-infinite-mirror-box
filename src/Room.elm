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
import RoomItem exposing (RoomItem)
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
    { rooms : NonemptyList RoomWithRay }

type alias RoomWithRay = 
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
    , trees : List RoomItem
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
    , trees = 
        [ RoomItem (Point2d.meters -0.5 0.3) RoomItem.emojis.roundTree
        , RoomItem (Point2d.meters 0.2 0.9) RoomItem.emojis.roundTree
        , RoomItem (Point2d.meters 1.1 -0.4) RoomItem.emojis.pineTree
        ]
    }

-- FUNCTIONS


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
        
        roomSvg = 
            Svg.g [] 
                [ Svg.polygon2d 
                    [ Attr.fill "none"
                    , Attr.strokeWidth "0.02"
                    , Attr.stroke "black"
                    ]
                    (model.wallShape |> Polygon2d.placeIn Shared.roomFrame)
                , viewRoomItem (Point2d.meters -0.5 0.3) RoomItem.emojis.roundTree
                , viewRoomItem (Point2d.meters 0.2 0.9) RoomItem.emojis.roundTree
                , viewRoomItem (Point2d.meters 1.1 -0.4) RoomItem.emojis.pineTree
                , viewRoomItem model.viewerPos RoomItem.emojis.cat
                , viewRoomItem model.targetPos RoomItem.emojis.parrot
                ]

        normalRay = 
            Sightray.fromRoomAndProjectedPath model.wallShape (projectedSightline model)

        (ray, centerPoint) = 
            normalRay
                |> Sightray.unravel
                |> Array.fromList
                |> (\rays -> Maybe.andThen (\s -> Array.get s rays) animationStage)
                |> Maybe.map (\r -> (r, Sightray.startPos r.start))
                |> Maybe.withDefault (normalRay, model.viewerPos)

                    -- (viewRayUnravelAnimationStep model.viewerPos ray 
    in
    Svg.g [] 
        [ viewRay ray
        , Svg.lineSegment2d 
            [ Attr.fill "none"
            , Attr.strokeWidth "0.03"
            , Attr.stroke "blue"
            ]
            (LineSegment2d.from model.viewerPos (Sightray.startPos ray.start))
        , roomSvg 
        ]
        |> Svg.translateBy (Vector2d.from centerPoint Point2d.origin)
        |> Svg.at (Shared.pixelsPerMeter 0.5)
        |> Svg.relativeTo Shared.topLeftFrame

viewRayUnravelAnimationStep : Point -> Sightray -> Int -> Svg Msg 
viewRayUnravelAnimationStep viewerPos ray step = 
    ray
        |> Sightray.unravel
        |> Array.fromList
        |> Array.get step
        |> Maybe.map (\tailRay -> 
            Svg.g []
                [ viewRay tailRay
                , Svg.lineSegment2d 
                    [ Attr.fill "none"
                    , Attr.strokeWidth "0.03"
                    , Attr.stroke "blue"
                    ]
                    (LineSegment2d.from viewerPos (Sightray.startPos tailRay.start))
                ] 
        )
        |> Maybe.withDefault (Svg.g [] [])


viewRay : Sightray -> Svg Msg 
viewRay ray = 
    ray 
        |> Sightray.vertices
        |> Polyline2d.fromVertices
        |> Svg.polyline2d
            [ Attr.fill "none" 
            , Attr.stroke "black"
            , Attr.strokeWidth "0.03"
            , Attr.strokeDasharray "0.05"
            ]