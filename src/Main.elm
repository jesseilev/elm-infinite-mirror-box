module Main exposing (..)

import Angle exposing (Angle)
import Array
import Axis2d exposing (Axis2d)
import Browser
import Circle2d
import Color
import Convert
import Direction2d exposing (Direction2d)
import Element as El exposing (Element)
import Element.Border as Border
import Element.Events
import VirtualDom
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Wheel as Wheel
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import List.Nonempty
import Maybe.Extra as Maybe
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity)
import RayPath exposing (RayPath)
import Room exposing (Model)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Convert exposing (lengthToPixels)
import Rectangle2d exposing (Rectangle2d)
import SketchPlane3d exposing (toPlane)
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types exposing (CoordinateSystem(..), Paint(..))
import Shared exposing (..)
import String exposing (startsWith)
import Vector2d exposing (Vector2d)


main = 
    Browser.element
        { init = init
        , update = update   
        , subscriptions = subscriptions
        , view = view
        }

-- MODEL --

type alias Model = 
    { room : Room.Model
    , mouseDragPos : Maybe Point
    , mouseDown : Bool
    , clickPosDebug : Point
    , zoomScale : Float
    , animationStage : Maybe Int
    }

init : () -> ( Model, Cmd Msg )
init _ =
    { room = Room.init1
    , mouseDragPos = Nothing
    , mouseDown = False
    , clickPosDebug = Point2d.origin
    , zoomScale = 0.3
    , animationStage = Just 0
    }
        |> noCmds


-- UPDATE --

type Msg 
    = NoOp
    | MouseDragAt (Point2d Meters SceneCoords)
    | ToggleMouseDown Bool
    | MouseClickAt (Point2d Meters SceneCoords)
    | AdjustZoom Float
    | RoomMsg Room.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        MouseDragAt mousePosInScene ->
            if not model.mouseDown then 
                model |> noCmds 
            else 
                { model 
                    | room = 
                        model.mouseDragPos
                            |> Maybe.map (\prevMouse -> 
                                Room.update (Room.mouseDragMsg prevMouse mousePosInScene) model.room)
                            |> Maybe.withDefault model.room
                    , mouseDragPos = Just mousePosInScene
                    , mouseDown = True
                }
                    |> noCmds

        ToggleMouseDown isDown -> 
            { model | mouseDown = isDown, mouseDragPos = Nothing }
                |> noCmds

        MouseClickAt sceneOffsetPos -> 
            { model 
                -- | viewerPos = sceneOffsetPos |> Point2d.relativeTo (roomFrame model)
                -- , clickPosDebug = sceneOffsetPos
                | animationStage = Maybe.map ((+) 1) model.animationStage
            }
                |> noCmds
        
        AdjustZoom deltaY ->
            { model  
                | zoomScale = model.zoomScale * (1.1 ^ (sign deltaY))
            }
                |> noCmds

        _ ->
            model |> noCmds


noCmds x = ( x, Cmd.none )


-- SUBSCRIPTIONS --
subscriptions _ = 
    Sub.none


-- VIEW --

view : Model -> Html Msg
view model = 
    El.layout 
        [ El.width El.fill
        , El.height El.fill 
        ]
        (El.el 
            [ El.centerX
            , El.centerY 
            , Border.solid
            , Border.width 2
            ] 
            (El.html (svgContainer model)))


svgContainer : Model -> Html Msg
svgContainer model =
    Html.div 
        [ Pointer.onDown (\_ -> ToggleMouseDown True)
        , Pointer.onUp (\_ -> ToggleMouseDown False |> Debug.log "mouse up!")
        , Pointer.onLeave (\_ -> ToggleMouseDown False |> Debug.log "mouse left!")
        , Pointer.onMove (\event -> 
            if model.mouseDown then 
                MouseDragAt (mouseToSceneCoords model event.pointer.offsetPos)
            else 
                NoOp)
        , Wheel.onWheel (\event -> AdjustZoom event.deltaY)
        , Mouse.onClick (\event -> MouseClickAt Point2d.origin)
        ]
        [ Svg.svg 
            [ Attr.width (constants.containerWidth |> String.fromFloat)
            , Attr.height (constants.containerHeight |> String.fromFloat)
            ]
            [ Room.view model.animationStage model.room 
                |> Svg.map RoomMsg
            ]
        , Html.text (model.mouseDragPos |> Maybe.map (\_ -> "dragging") |> Maybe.withDefault "no drag")
        ]

roomItem : Model -> String -> Point -> Svg Msg
roomItem model emoji pos = 
    let
        fontSize = 0.25
    in
    Svg.g [] 
        [ Svg.circle2d
            [ Mouse.onClick (\event -> MouseClickAt (mouseToSceneCoords model event.offsetPos))
            , TypedSvg.Attributes.fill <| Paint Color.white
            , Attr.strokeWidth "0.01"
            , Attr.stroke "lightGrey"
            ]
            (Circle2d.atOrigin (Length.meters fontSize))
        , Svg.text_ 
            [ Attr.fontSize (String.fromFloat fontSize)
            , Attr.x (-0.5 * fontSize |> String.fromFloat)
            , Attr.alignmentBaseline "central"
            ] 
            [ Svg.text emoji ]
            |> Svg.mirrorAcross (Axis2d.through Point2d.origin Direction2d.x)
        ]
        |> Svg.translateBy (Vector2d.from Point2d.origin pos)


frameDebugViz : String -> Svg Msg        
frameDebugViz clr = 
    Svg.g [] 
        [ Svg.rectangle2d 
            [ Attr.stroke clr
            , Attr.strokeWidth "0.02" -- in meters. TODO use typedsvg
            , VirtualDom.attribute "vector-effect" "non-scaling-stroke"
            , Attr.fill "none" 
            ]
            (Rectangle2d.from Point2d.origin (Point2d.meters 0.5 1.0))
        , Svg.circle2d [ Attr.fill "black" ]
            (Circle2d.atOrigin (Length.meters 0.05))
        ]

    

-- Frame, Units, Conversions --

pixelsPerMeter model = 
    pixels 100 
        |> Quantity.per (Length.meters 1)
        |> Quantity.multiplyBy model.zoomScale

topLeftFrame : Model -> Frame2d Pixels SceneCoords { defines : TopLeftCoords }
topLeftFrame model = 
    Frame2d.atOrigin
        |> Frame2d.translateBy
            (Vector2d.xy
                (pixels <| -constants.containerWidth / 2.0)
                (pixels <| constants.containerHeight / 2.0))
        |> Frame2d.reverseY

mouseToSceneCoords : Model -> (Float, Float) -> Point2d Meters SceneCoords
mouseToSceneCoords model (x, y) = 
    Point2d.pixels x y
        |> Point2d.placeIn (topLeftFrame model)
        |> Point2d.at_ (pixelsPerMeter model)

svgToSceneCoords : Frame2d Pixels globalC { defines : localC } -> Svg msg -> Svg msg
svgToSceneCoords localFrame svg =
    svg 
        |> Svg.mirrorAcross Axis2d.x 
        |> Svg.placeIn localFrame

pointYAxisAt : Point2d u c -> Frame2d u c {} -> Frame2d u c {}
pointYAxisAt target frame =
    let 
        currentDirection =
            Frame2d.yDirection frame

        targetDirection = 
            Vector2d.from (Frame2d.originPoint frame) target
                |> Vector2d.direction
                |> Maybe.withDefault currentDirection

        angleDifference = 
            Direction2d.angleFrom currentDirection targetDirection
    in 
        Frame2d.rotateBy angleDifference frame

maybePair : Maybe a -> Maybe b -> Maybe (a, b)
maybePair ma mb = 
    case (ma, mb) of 
        (Just x, Just y) -> Just (x, y)
        _ -> Nothing

sign : number -> number
sign n = 
    if n < 0 then -1 else 1


-- COMPUTING STUFF --



