module RoomItem exposing 
    ( RoomItem
    , init
    , setPos
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
import Length
import Point2d
import Shared exposing (Point)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import TypedSvg.Attributes
import TypedSvg.Types exposing (Paint(..))
import Vector2d 
import Shared exposing (InterpolatedReflection)
import Shared exposing (interpReflectPoint)

type alias RoomItem = 
    { pos : Point 
    , emoji : String 
    }

init : Point -> String -> RoomItem
init = 
    RoomItem

setPos : Point -> RoomItem -> RoomItem
setPos pos item = 
    updatePos (\_ -> pos) item

updatePos : (Point -> Point) -> RoomItem -> RoomItem
updatePos upd item = 
    { item | pos = upd item.pos }


-- TODO make typesafe
emojis = 
    { roundTree = "🌳"
    , pineTree = "🌲"
    , palmTree = "🌴"
    , cat = "🐈‍⬛"
    , parrot = "🦜"
    }

-- UPDATE

type Msg = NoOp


-- VIEW


view : RoomItem -> Svg Msg
view item = 
    let
        fontSize = 0.25
    in
    Svg.g [] 
        [ Svg.circle2d
            [ TypedSvg.Attributes.fill <| Paint Color.white
                -- Mouse.onClick (\event -> MouseClickAt (mouseToSceneCoords model event.offsetPos))
            , Attr.strokeWidth "0.01"
            , Attr.stroke "lightGrey"
            ]
            (Circle2d.atOrigin (Length.meters fontSize))
        , Svg.text_ 
            [ Attr.fontSize (String.fromFloat fontSize)
            , Attr.x (-0.5 * fontSize |> String.fromFloat)
            , Attr.alignmentBaseline "central"
            ] 
            [ Svg.text item.emoji ]
            |> Svg.mirrorAcross (Axis2d.through Point2d.origin Direction2d.x)
        ]
        |> Svg.translateBy (Vector2d.from Point2d.origin item.pos)


interpReflect : InterpolatedReflection RoomItem
interpReflect axis pct item =
    item |> updatePos (interpReflectPoint axis pct)