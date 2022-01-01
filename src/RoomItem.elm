module RoomItem exposing 
    ( containsPoint
    , emojis
    , create
    , interpolateFrom
    , mirrorAcross
    , radius
    , RoomItem
    , setPos
    , view
    )

import Axis2d
import Circle2d
import Direction2d
import Geometry.Svg as Svg
import Length exposing (Length)
import Point2d
import Shared exposing (Point)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import TypedSvg.Types exposing (Paint(..))
import Vector2d 
import Shared exposing (Axis, Circle)
import Quantity
import Circle2d


-- Types --

type alias RoomItem = 
    { pos : Point 
    , emoji : String 
    }


-- Constructors --

create : Point -> String -> RoomItem
create = 
    RoomItem


-- Transformations --

setPos : Point -> RoomItem -> RoomItem
setPos pos item = 
    updatePos (\_ -> pos) item

updatePos : (Point -> Point) -> RoomItem -> RoomItem
updatePos upd item = 
    { item | pos = upd item.pos }

setEmoji emoji item =
    { item | emoji = emoji }


-- Properties --

containsPoint : Point -> RoomItem -> Bool
containsPoint p item =
    Circle2d.contains p (boundaryCircle item)

boundaryCircle : RoomItem -> Circle
boundaryCircle item = 
    Circle2d.atPoint item.pos radius

interpolateFrom : Shared.Interpolation RoomItem 
interpolateFrom item1 item2 pct = 
    item1 
        |> updatePos (\p1 -> Shared.interpolatePointFrom p1 item2.pos pct)
        |> setEmoji (if pct < 0.5 then item1.emoji else item2.emoji)

mirrorAcross : Axis -> RoomItem -> RoomItem
mirrorAcross axis = 
    updatePos (Point2d.mirrorAcross axis)

radius : Length 
radius = 
    Length.meters 0.2

emojis = 
    { roundTree = "🌳"
    , pineTree = "🌲"
    , palmTree = "🌴"
    , plant = "🌱"
    , cat = "🐈‍"
    , camera = "📷"
    , cameraFlash = "📸"
    , cameraVid = "🎥"
    , cameraVid2 = "📹"
    , parrot = "🦜"
    }



-- VIEW

view : RoomItem -> Svg msg
view item = 
    let
        fontSize = (Quantity.unwrap radius) * 0.95
    in
    Svg.g 
        [ ] 
        [ Svg.circle2d
            [ Attr.strokeWidth <| "0.01"
            , Attr.stroke <| Shared.colors.greyVeryLight
            , Attr.fill <| "none"
            ]
            (boundaryCircle item)
        , Svg.text_ 
            [ Attr.fontSize (String.fromFloat fontSize)
            , Attr.x (-0.5 * fontSize |> String.fromFloat)
            , Attr.alignmentBaseline "central"
            ] 
            [ Svg.text item.emoji ]
            |> Svg.mirrorAcross (Axis2d.through Point2d.origin Direction2d.x)
            |> Svg.translateBy (Vector2d.from Point2d.origin item.pos)
        ]

