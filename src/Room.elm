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
    , Direction
    , LineSegment
    , Point 
    , Polygon 
    , SceneCoords
    , TopLeftCoords
    , InterpolatedReflection
    , interpReflectPoint
    , interpReflectPolygon
    , interpReflectPolyline
    )
import Vector2d
import Direction2d exposing (Direction2d)
import Polyline2d exposing (Polyline2d)
import RoomItem exposing (interpReflect)

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
    , viewerDirection : Direction 
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
    , viewerDirection = Direction2d.fromAngle (Angle.degrees 50)
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
            (Frame2d.yDirection (Shared.viewerFrame model.viewerPos 
                (Direction2d.toAngle model.viewerDirection))))


angleDiff : Point2d u c -> Point2d u c -> Point2d u c -> Maybe Angle
angleDiff pivot p1 p2 = 
    let
        getAngle p = 
            Direction2d.from pivot p 
                |> Maybe.map Direction2d.toAngle
    in
        Maybe.map2 Quantity.difference (getAngle p2) (getAngle p1)

interpReflect : InterpolatedReflection Model 
interpReflect axis pct model =
    { model 
        | wallShape = Shared.interpReflectPolygon axis pct model.wallShape
        , viewerPos = Shared.interpReflectPoint axis pct model.viewerPos
        , viewerDirection = Shared.interpReflectDirection axis pct model.viewerDirection
        , targetPos = Shared.interpReflectPoint axis pct model.targetPos
        , trees = List.map (RoomItem.interpReflect axis pct) model.trees
    }


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
    let viewerAngle = Direction2d.toAngle model.viewerDirection in
    case msg of 
        MouseDragMsg prevMousePos mousePos -> 
            Shared.viewerFrame model.viewerPos viewerAngle
                |> Frame2d.originPoint
                |> (\origin -> angleDiff origin prevMousePos mousePos)
                |> Maybe.map (Quantity.plus viewerAngle)
                |> Maybe.withDefault viewerAngle
                |> (\a -> { model | viewerDirection = Direction2d.fromAngle a })
        _ -> 
            model

-- VIEW

view : Maybe Shared.SuccessAnimation -> Model -> Svg Msg 
view animation model = 
    Svg.g [] 
        [ view_ animation model 
        , animation 
            |> Maybe.map (\a -> 
                view_ animation
                    (model |> interpReflect fakeAxis (a.transitionPct |> Maybe.withDefault 0))
            )
            |> Maybe.withDefault (Svg.g [] [])
        ]
        -- |> Svg.translateBy (Vector2d.from centerPoint Point2d.origin)
        |> Svg.at (Shared.pixelsPerMeter 0.3)
        |> Svg.relativeTo Shared.topLeftFrame

view_ : Maybe Shared.SuccessAnimation -> Model -> Svg Msg 
view_ animation model = 
    let 
        viewRoomItem item = 
            RoomItem.init item.pos item.emoji
                |> RoomItem.view
                |> Svg.map RoomItemMsg
        
        roomSvg = 
            Svg.g [] 
                <| [ Svg.polygon2d 
                    [ Attr.fill "none"
                    , Attr.strokeWidth "0.02"
                    , Attr.stroke "black"
                    ]
                    (model.wallShape |> Polygon2d.placeIn Shared.roomFrame)
                , viewRoomItem <| RoomItem.init model.viewerPos RoomItem.emojis.cat
                , viewRoomItem <| RoomItem.init model.targetPos RoomItem.emojis.parrot
                ]
                ++ (List.map viewRoomItem model.trees)

        normalRay = 
            Sightray.fromRoomAndProjectedPath model.wallShape (projectedSightline model)

        (ray, centerPoint) = 
            normalRay
                |> Sightray.unravel
                |> Array.fromList
                |> (\rays -> Maybe.andThen (\s -> Array.get s rays) (Maybe.map .step animation))
                |> Maybe.map (\r -> (r, Sightray.startPos r.start))
                |> Maybe.withDefault (normalRay, model.viewerPos)

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
        -- , Sightray.vertices ray
        --     |> Polyline2d.fromVertices
        --     |> Shared.interpReflectPolyline fakeAxis fakePct
        --     |> Svg.polyline2d [ Attr.fill "none", Attr.stroke "red", Attr.strokeWidth "0.02" ]
        -- , model.wallShape
        --     |> Shared.interpReflectPolygon fakeAxis fakePct
        --     |> Svg.polygon2d [ Attr.fill "none", Attr.stroke "red", Attr.strokeWidth "0.02" ]
        ]

fakeAxis = Axis2d.through (Point2d.meters 3 0) Direction2d.y

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