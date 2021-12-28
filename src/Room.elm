module Room exposing 
    ( Model
    , init1
    , setStatusMsg
    , allItems
    , playerItem
    , Status(..)
    , Msg
    , update
    , mouseDragMsg
    , view
    , projectedSightline
    , interpReflect
    , interpolateFrom
    )

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

-- type alias Hallway =
--     { rooms : NonemptyList RoomWithRay }

-- type alias RoomWithRay = 
--     { roomStuff : () 
--     , sightray : Sightray
--     }


type alias NonemptyList a = List.Nonempty.Nonempty a


type alias Model = 
    { wallShape : Polygon
    , viewerPos : Point
    , viewerDirection : Direction 
    , status : Status
    , sightDistance : Quantity Float Meters
    , targetPos : Point
    , trees : List RoomItem
    }

type Status = Standing | LookingAround | TakingPic

init1 : Model 
init1 =
    { wallShape = 
        Polygon2d.singleLoop 
            [ Point2d.meters -2.0 -2.0
            , Point2d.meters 2.0 -1.0
            , Point2d.meters 2.25 1.0
            , Point2d.meters -1.5 2.25
            ]
    , viewerPos = Point2d.meters 0.35 -0.65
    , viewerDirection = Direction2d.fromAngle (Angle.degrees -15)
    , status = Standing
    , sightDistance = Length.meters 8.0
    , targetPos = Point2d.meters 1.5 0.2
    , trees = 
        [ RoomItem (Point2d.meters -0.5 0.3) RoomItem.emojis.plant
        , RoomItem (Point2d.meters 0.2 0.9) RoomItem.emojis.plant
        , RoomItem (Point2d.meters 1.1 -0.4) RoomItem.emojis.plant
        ]
    }

-- TODO just make these things actual items
targetItem : Model -> RoomItem 
targetItem model = 
    RoomItem.init model.targetPos RoomItem.emojis.parrot

playerItem : Model -> RoomItem 
playerItem model = 
    ( case model.status of 
        Standing -> RoomItem.emojis.cat
        LookingAround -> RoomItem.emojis.cameraVid2 
        TakingPic -> RoomItem.emojis.cameraFlash
    )
        |> RoomItem.init model.viewerPos

-- FUNCTIONS


projectedSightline : Model -> LineSegment
projectedSightline model =
    LineSegment2d.fromPointAndVector model.viewerPos
        (Vector2d.withLength model.sightDistance 
            (Frame2d.yDirection (Shared.viewerFrame model.viewerPos 
                (Direction2d.toAngle model.viewerDirection))))


interpReflect : InterpolatedReflection Model 
interpReflect axis pct model =
    { model 
        | wallShape = Shared.interpReflectPolygon axis pct model.wallShape
        , viewerPos = Shared.interpReflectPoint axis pct model.viewerPos
        , viewerDirection = Shared.interpReflectDirection axis pct model.viewerDirection
        , targetPos = Shared.interpReflectPoint axis pct model.targetPos
        , trees = List.map (RoomItem.interpReflect axis pct) model.trees
    }

interpolateFrom : Model -> Model -> Float -> Model
interpolateFrom r1 r2 pct = 
    { r1 
        | wallShape = Shared.interpolatePolygonFrom r1.wallShape r2.wallShape pct
        , viewerPos = Shared.interpolatePointFrom r1.viewerPos r2.viewerPos pct
        , viewerDirection = Shared.interpolateDirectionFrom r1.viewerDirection r2.viewerDirection pct
        , targetPos = Shared.interpolatePointFrom r1.targetPos r2.targetPos pct
        , trees = Shared.interpolateLists RoomItem.interpolateFrom r1.trees r2.trees pct
    }

allItems : Model -> List RoomItem
allItems model = 
    model.trees ++ [ targetItem model, playerItem model ]

-- UPDATE

type Msg 
    = NoOp
    | MouseDragMsg Point Point
    | SetStatus Status
    | RoomItemMsg RoomItem.Msg

mouseDragMsg : Point -> Point -> Msg
mouseDragMsg = 
    MouseDragMsg

setStatusMsg : Status -> Msg 
setStatusMsg = 
    SetStatus

update : Msg -> Model -> Model
update msg model =
    let viewerAngle = Direction2d.toAngle model.viewerDirection in
    case msg of 
        MouseDragMsg prevMousePos mousePos -> 
            Shared.viewerFrame model.viewerPos viewerAngle
                |> Frame2d.originPoint
                |> (\origin -> Shared.angleDiff origin prevMousePos mousePos)
                |> Maybe.map (Quantity.plus viewerAngle)
                |> Maybe.withDefault viewerAngle
                |> (\a -> { model | viewerDirection = Direction2d.fromAngle a })

        SetStatus status ->
            { model | status = status }

        _ -> 
            model

-- VIEW

view : Model -> Svg Msg 
view model = 
    let 
        viewRoomItem flag item = 
            RoomItem.init item.pos item.emoji
                |> RoomItem.view flag
                |> Svg.map RoomItemMsg

        roomSvg =
            Svg.g [] <| 
                [ Svg.polygon2d 
                    [ Attr.fill "none"
                    , Attr.strokeWidth "0.04"
                    , Attr.stroke "black"
                    ]
                    (model.wallShape |> Polygon2d.placeIn Shared.roomFrame)
                , playerItem model 
                    |> viewRoomItem False
                    |> Svg.rotateAround model.viewerPos 
                        (if model.status == Standing then 
                            Angle.degrees 0
                        else 
                         Direction2d.toAngle model.viewerDirection |> Quantity.minus (Angle.degrees 90)
                        )
                , viewRoomItem True <| targetItem model
                ]
                ++ (List.map (viewRoomItem False) model.trees)        
    in
        roomSvg 
            -- |> Svg.at (Shared.pixelsPerMeter 0.3)
            -- |> Svg.relativeTo Shared.topLeftFrame
