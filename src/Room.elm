module Room exposing 
    ( Model
    , init1
    , allItems
    , playerItem
    , targetItem
    , view
    , interpReflect
    , mirrorAcross
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
    )
import Vector2d
import Direction2d exposing (Direction2d)
import Polyline2d exposing (Polyline2d)
import RoomItem exposing (interpReflect)


type alias NonemptyList a = List.Nonempty.Nonempty a


type alias Model = 
    { wallShape : Polygon
    , playerItem : RoomItem
    , targetItem : RoomItem
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
    , playerItem = RoomItem (Point2d.meters 0.35 -0.65) RoomItem.emojis.cat
    , targetItem = RoomItem (Point2d.meters 1.5 0.2) RoomItem.emojis.parrot
    , trees =
        -- []
        [ RoomItem (Point2d.meters -0.5 0.3) RoomItem.emojis.plant
        , RoomItem (Point2d.meters 0.2 0.9) RoomItem.emojis.plant
        , RoomItem (Point2d.meters 1.1 -0.4) RoomItem.emojis.plant
        ]
    }

-- TODO just make these things actual items
playerItem : Model -> RoomItem 
playerItem = 
    .playerItem
    -- ( case model.status of 
    --     Standing -> RoomItem.emojis.cat
    --     LookingAround -> RoomItem.emojis.cameraVid2 
    --     TakingPic -> RoomItem.emojis.cameraFlash
    -- )
        -- |> 
        -- RoomItem.init model.viewerPos emoji


targetItem : Model -> RoomItem 
targetItem = 
    .targetItem 

-- FUNCTIONS


interpReflect : Shared.InterpolatedReflection Model 
interpReflect axis pct model =
    { wallShape = Shared.interpReflectPolygon axis pct model.wallShape
    , playerItem = RoomItem.interpReflect axis pct model.playerItem
    , targetItem = RoomItem.interpReflect axis pct model.targetItem
    , trees = List.map (RoomItem.interpReflect axis pct) model.trees
    }

mirrorAcross : Axis -> Model -> Model 
mirrorAcross axis = 
    interpReflect axis 1 -- TODO dont use the interp

interpolateFrom : Model -> Model -> Float -> Model
interpolateFrom r1 r2 pct = 
    { wallShape = Shared.interpolatePolygonFrom r1.wallShape r2.wallShape pct
    , playerItem = RoomItem.interpolateFrom r1.playerItem r2.playerItem pct
    , targetItem = RoomItem.interpolateFrom r1.targetItem r2.targetItem pct
    , trees = Shared.interpolateLists RoomItem.interpolateFrom r1.trees r2.trees pct
    }

allItems : Model -> List RoomItem
allItems model = 
    model.trees ++ [ model.targetItem, model.playerItem ]

-- VIEW

view : Float -> Model -> Svg msg 
view zoomScale model = 
    let 
        viewRoomItem flag item = 
            RoomItem.init item.pos item.emoji
                |> RoomItem.view flag

        roomSvg =
            Svg.g [] <| 
                [ Svg.polygon2d 
                    [ Attr.fill Shared.colors.roomBackground
                    , Attr.strokeWidth <| Shared.floatAttribute zoomScale 0.03
                    , Attr.stroke "grey"
                    -- , Attr.opacity "0.5"
                    ]
                    (model.wallShape |> Polygon2d.placeIn Shared.roomFrame)
                , viewRoomItem False model.playerItem
                    -- |> Svg.rotateAround model.viewerPos 
                    --     (if model.status == Standing then 
                    --         Angle.degrees 0
                    --     else 
                    --      Direction2d.toAngle model.viewerDirection |> Quantity.minus (Angle.degrees 90)
                    --     )
                , viewRoomItem True model.targetItem
                ]
                ++ (List.map (viewRoomItem False) model.trees)        
    in
        roomSvg 
            -- |> Svg.at (Shared.pixelsPerMeter 0.3)
            -- |> Svg.relativeTo Shared.topLeftFrame



-- type Msg 
--     = NoOp
--     | MouseDragMsg Point Point
--     | SetStatus Status
--     | RoomItemMsg RoomItem.Msg

-- mouseDragMsg : Point -> Point -> Msg
-- mouseDragMsg = 
--     MouseDragMsg

-- setStatusMsg : Status -> Msg 
-- setStatusMsg = 
--     SetStatus

-- update : Msg -> Model -> Model
-- update msg model =
--     let viewerAngle = Direction2d.toAngle model.viewerDirection in
--     case msg of 
--         MouseDragMsg prevMousePos mousePos -> 
--             Shared.viewerFrame model.viewerPos viewerAngle
--                 |> Frame2d.originPoint
--                 |> (\origin -> Shared.angleDiff origin prevMousePos mousePos)
--                 |> Maybe.map (Quantity.plus viewerAngle)
--                 |> Maybe.withDefault viewerAngle
--                 |> (\a -> { model | viewerDirection = Direction2d.fromAngle a })

--         SetStatus status ->
--             { model | status = status }

--         _ -> 
--             model