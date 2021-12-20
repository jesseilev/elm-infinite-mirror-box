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
import Element.Input as Input
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
import Time

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
    , successAnimation : Maybe Shared.SuccessAnimation
    }


init : () -> ( Model, Cmd Msg )
init _ =
    { room = Room.init1
    , mouseDragPos = Nothing
    , mouseDown = False
    , clickPosDebug = Point2d.origin
    , zoomScale = 0.3
    , successAnimation = Just (SuccessAnimation 0 (Just 0.4))
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
    | StepAnimation Int
    | Tick

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

        -- MouseClickAt sceneOffsetPos -> 
        --     { model 
        --         -- | viewerPos = sceneOffsetPos |> Point2d.relativeTo (roomFrame model)
        --         -- , clickPosDebug = sceneOffsetPos
        --     }
        --         |> noCmds
        
        AdjustZoom deltaY ->
            { model  
                | zoomScale = model.zoomScale * (1.1 ^ (sign deltaY))
            }
                |> noCmds

        StepAnimation stepDiff ->
            { model | successAnimation = model.successAnimation 
                |> Maybe.map (\ani -> 
                    { ani | step = ani.step + stepDiff, transitionPct = Just 0 }
                )
            }
                |> noCmds

        Tick -> 
            { model | successAnimation = model.successAnimation 
                |> Maybe.map (\ani -> 
                    case ani.transitionPct of 
                        Nothing -> ani
                        Just pct ->
                            { ani | transitionPct = 
                                pct + 0.02
                                    |> (\newPct -> 
                                        if newPct < 1.0 then 
                                            Just newPct
                                        else 
                                            Nothing
                                        )
                            }
                )
            }
                |> noCmds

        _ ->
            model |> noCmds


-- SUBSCRIPTIONS --

subscriptions : Model -> Sub Msg
subscriptions model = 
    case model.successAnimation |> Maybe.map .transitionPct of
        Just _ ->
            Time.every 50 (\_ -> Tick)
        _ ->
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
            ] 
            (El.column []
                [ El.el 
                    [ Border.solid
                    , Border.width 2
                    ] 
                    (El.html (svgContainer model))
                , viewAnimationButtons model
                ]
            )
        )


svgContainer : Model -> Html Msg
svgContainer model =
    Html.div 
        [ Pointer.onDown (\_ -> ToggleMouseDown True)
        , Pointer.onUp (\_ -> ToggleMouseDown False |> Debug.log "mouse up!")
        , Pointer.onLeave (\_ -> ToggleMouseDown False |> Debug.log "mouse left!")
        , Pointer.onMove (\event -> 
            if model.mouseDown then 
                MouseDragAt (mouseToSceneCoords model.zoomScale event.pointer.offsetPos)
            else 
                NoOp)
        , Wheel.onWheel (\event -> AdjustZoom event.deltaY)
        , Mouse.onClick (\event -> if model.mouseDown then NoOp else MouseClickAt Point2d.origin)
        ]
        [ Svg.svg 
            [ Attr.width (constants.containerWidth |> String.fromFloat)
            , Attr.height (constants.containerHeight |> String.fromFloat)
            ]
            [ Room.view model.successAnimation model.room 
                |> Svg.map RoomMsg
            ]
        ]

viewAnimationButtons : Model -> Element Msg 
viewAnimationButtons model = 
    El.row 
        [ El.centerX 
        , El.spacing 20 
        ]
        [ Input.button []
            { onPress = Just (StepAnimation -1)
            , label = El.text "<"
            }
        , Input.button []
            { onPress = Just (StepAnimation 1)
            , label = El.text ">"
            }
        ]

-- Frame, Units, Conversions --

pixelsPerMeter zoomScale = 
    pixels 100 
        |> Quantity.per (Length.meters 1)
        |> Quantity.multiplyBy zoomScale

topLeftFrame : Frame2d Pixels SceneCoords { defines : TopLeftCoords }
topLeftFrame = 
    Frame2d.atOrigin
        |> Frame2d.translateBy
            (Vector2d.xy
                (pixels <| -constants.containerWidth / 2.0)
                (pixels <| constants.containerHeight / 2.0))
        |> Frame2d.reverseY

mouseToSceneCoords : Float -> (Float, Float) -> Point2d Meters SceneCoords
mouseToSceneCoords zoomScale (x, y) = 
    Point2d.pixels x y
        |> Point2d.placeIn topLeftFrame
        |> Point2d.at_ (pixelsPerMeter zoomScale)

svgToSceneCoords : Frame2d Pixels globalC { defines : localC } -> Svg msg -> Svg msg
svgToSceneCoords localFrame svg =
    svg 
        |> Svg.mirrorAcross Axis2d.x 
        |> Svg.placeIn localFrame


