module RoomItem exposing 
    ( RoomItem
    , init
    , setPos
    , containsPoint
    , radius
    , emojis
    , view
    , interpReflect
    , interpolateFrom
    )

import Axis2d
import Circle2d
import Direction2d
import Color
import Geometry.Svg as Svg
import Length exposing (Length)
import Point2d
import Shared exposing (Point)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import TypedSvg.Attributes
import TypedSvg.Types exposing (Paint(..))
import Vector2d 
import Shared exposing (InterpolatedReflection)
import Shared exposing (Circle, interpReflectPoint)
import Quantity exposing (Quantity)
import Circle2d exposing (Circle2d)
import Circle2d exposing (boundingBox)

type alias RoomItem = 
    { pos : Point 
    , emoji : String 
    }

init : Point -> String -> RoomItem
init = 
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

radius : Length 
radius = 
    Length.meters 0.2

interpReflect : InterpolatedReflection RoomItem
interpReflect axis pct item =
    item |> updatePos (interpReflectPoint axis pct)

interpolateFrom : Shared.Interpolation RoomItem 
interpolateFrom item1 item2 pct = 
    item1 
        |> updatePos (\p1 -> Shared.interpolatePointFrom p1 item2.pos pct)
        |> setEmoji (if pct < 0.5 then item1.emoji else item2.emoji)


-- TODO make typesafe
emojis = 
    { roundTree = "🌳"
    , pineTree = "🌲"
    , palmTree = "🌴"
    , plant = "🪴"
    , cat = "🐈‍"
    , camera = "📷"
    , cameraFlash = "📸"
    , cameraVid = "🎥"
    , cameraVid2 = "📹"
    , parrot = "🦜"
    }



-- VIEW


view : Bool -> RoomItem -> Svg msg
view inFocus item = 
    let
        fontSize = (Quantity.unwrap radius) * 0.95
    in
    Svg.g 
        [ ] 
        [ Svg.circle2d
            [ Attr.strokeWidth <| "0.01" --if inFocus then "0.01" else "0"
            , Attr.stroke <| Shared.colors.greyVeryLight --if inFocus then Shared.colors.yellow1 else "#f0f0f0"
            , Attr.fill <| "none" --if inFocus then Shared.colors.yellow1 else "white"
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

