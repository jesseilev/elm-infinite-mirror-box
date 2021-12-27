module RoomItem exposing 
    ( RoomItem
    , init
    , setPos
    , containsPoint
    , radius
    , emojis
    , Msg
    , view
    , interpReflect
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



-- Properties --

containsPoint : Point -> RoomItem -> Bool
containsPoint p item =
    Circle2d.contains p (boundaryCircle item)

boundaryCircle : RoomItem -> Circle
boundaryCircle item = 
    Circle2d.atPoint item.pos radius

radius : Length 
radius = 
    Length.meters 0.25

interpReflect : InterpolatedReflection RoomItem
interpReflect axis pct item =
    item |> updatePos (interpReflectPoint axis pct)

-- TODO make typesafe
emojis = 
    { roundTree = "🌳"
    , pineTree = "🌲"
    , palmTree = "🌴"
    , plant = "🪴"
    , cat = "🐈‍⬛"
    , camera = "📷"
    , cameraFlash = "📸"
    , parrot = "🦜"
    }

-- UPDATE

type Msg = NoOp


-- VIEW


view : Bool -> RoomItem -> Svg Msg
view inFocus item = 
    let
        fontSize = Quantity.unwrap radius 
    in
    Svg.g [] 
        [ Svg.circle2d
            [ TypedSvg.Attributes.fill <| Paint Color.white
                -- Mouse.onClick (\event -> MouseClickAt (mouseToSceneCoords model event.offsetPos))
            , Attr.strokeWidth <| if inFocus then "0.01" else "0"
            , Attr.fill "#f7f7f7"
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

