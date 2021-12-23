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
import List.Extra as List
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
import Sightray
import String exposing (startsWith)
import Vector2d exposing (Vector2d)
import Time
import Sightray exposing (startPos)
import Sightray exposing (Sightray)
import Sightray exposing (interpReflect)
import Circle2d exposing (centerPoint)
import TypedSvg.Types exposing (YesNo(..))
import Room exposing (projectedSightline)
import RoomItem

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
    , zoomScale : Float
    , successAnimation : Maybe Shared.SuccessAnimation
    }



init : () -> ( Model, Cmd Msg )
init _ =
    { room = Room.init1
    , mouseDragPos = Nothing
    , mouseDown = False
    , zoomScale = 0.3
    , successAnimation = Nothing --Just (SuccessAnimation 0 Nothing)
    }
        |> noCmds


-- UPDATE --

type Msg 
    = NoOp
    | MouseDragAt (Point2d Meters SceneCoords)
    | MouseDown
    | MouseUp
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

        MouseDown -> 
            { model | mouseDown = True, mouseDragPos = Nothing }
                |> noCmds

        MouseUp -> 
            { model | mouseDown = False, mouseDragPos = Nothing }
                |> checkSuccess
                |> noCmds

        MouseClickAt sceneOffsetPos -> 
            model 
                |> checkSuccess
                |> Debug.log "checked success"
                |> noCmds
        
        AdjustZoom deltaY ->
            { model  
                | zoomScale = model.zoomScale * (1.1 ^ (sign deltaY))
            }
                |> noCmds

        StepAnimation stepDiff ->
            { model | successAnimation = model.successAnimation 
                |> Maybe.map (\ani -> 
                    { ani | step = ani.step + 1, transitionPct = Just 0 }
                )
            }
                |> noCmds

        Tick -> 
            { model | successAnimation = model.successAnimation 
                |> Maybe.map (\ani -> 
                    case ani.transitionPct of 
                        Nothing -> ani
                        Just pct -> pct + 0.02 |> (\newPct -> 
                            if newPct < 1.0 then 
                                { ani | transitionPct = Just newPct }
                            else 
                                { ani | transitionPct = Nothing, step = ani.step }
                            )
                )
            }
                |> noCmds

        _ ->
            model |> noCmds

checkSuccess : Model -> Model 
checkSuccess model = 
    let 
        sightEnd = 
            rayNormal model |> .end |> Sightray.endPos 

        targetHit = 
            RoomItem.containsPoint sightEnd (targetItem model)

        newAnimation = 
            case model.successAnimation of 
                Nothing -> 
                    if targetHit then Just (Shared.SuccessAnimation 0 Nothing) else Nothing
                Just ani -> Just ani
    in
        { model | successAnimation = newAnimation }

-- TODO just make it an actual Item in the first place
targetItem model =
    RoomItem.init model.room.targetPos RoomItem.emojis.parrot


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
        [ Pointer.onDown (\_ -> MouseDown)
        , Pointer.onUp (\_ -> MouseUp)
        , Pointer.onLeave (\_ -> MouseUp)
        , Pointer.onMove (\event -> 
            if model.mouseDown then 
                MouseDragAt (mouseToSceneCoords model.zoomScale event.pointer.offsetPos)
            else 
                NoOp)
        -- , Wheel.onWheel (\event -> AdjustZoom event.deltaY)
        -- , Mouse.onClick (\event -> if model.mouseDown then NoOp else MouseClickAt Point2d.origin)
        ]
        [ Svg.svg 
            [ Attr.width (constants.containerWidth |> String.fromFloat)
            , Attr.height (constants.containerHeight |> String.fromFloat)
            ]
            [ Svg.g [] 
                [ model.successAnimation 
                    |> Maybe.map (viewDiagramSuccess model)
                    |> Maybe.withDefault (viewDiagramNormal model)
                ]
            ]
        ]

viewDiagramNormal model =
    Svg.g []
        [ viewRayNormal model
        , Room.view model.room |> Svg.map RoomMsg
        ]
        |> Svg.at Shared.pixelsPerMeter
        |> Svg.relativeTo Shared.topLeftFrame
        
viewDiagramSuccess : Model -> Shared.SuccessAnimation -> Svg Msg
viewDiagramSuccess model animation = 
    let 
        ray = raySuccess model animation 
        rooms =
            reflectedRooms (projectedSightline model.room) model.room []
                |> List.reverse
                |> List.take (animation.step + 1) 

        focusPoint = 
            rooms 
                |> List.map (.wallShape >> Polygon2d.vertices)
                |> List.concat
                |> Polygon2d.convexHull
                |> Polygon2d.centroid
                |> Maybe.withDefault (Sightray.startPos ray.start)
    in
    Svg.g [ ] 
        [ raySuccess model animation
            |> Sightray.view
        , Svg.lineSegment2d [ Attr.fill "none", Attr.strokeWidth "0.03", Attr.stroke "black" ]
            (LineSegment2d.from model.room.viewerPos (Sightray.startPos ray.start))
        , rooms
            |> List.map (Room.view >> Svg.map RoomMsg)
            |> Svg.g [ Attr.opacity "0.3" ]
        , Room.view model.room |> Svg.map RoomMsg
        ]
        |> Svg.translateBy (Vector2d.from focusPoint Point2d.origin)
        |> Svg.at (Shared.pixelsPerMeter 
            |> Quantity.multiplyBy (toFloat (animation.step + 1) ^ -0.9)
        )
        |> Svg.relativeTo Shared.topLeftFrame

reflectedRooms : LineSegment -> Room.Model -> List Room.Model -> List Room.Model
reflectedRooms sightline room roomsAcc = 
    Sightray.nextBounce room.wallShape sightline
        |> Maybe.map (\bounce -> 
            reflectedRooms 
                (LineSegment2d.from bounce.point (LineSegment2d.endPoint sightline))
                (room |> Room.interpReflect bounce.axis 1)
                (room :: roomsAcc))
        |> Maybe.withDefault (room :: roomsAcc)

type ApparentRoom 
    = ActualRoom Room.Model
    | ReflectionOf ApparentRoom Sightray.MirrorBounce

computeApparentRoom : ApparentRoom -> Room.Model 
computeApparentRoom ar =
    case ar of 
        ActualRoom room -> room
        ReflectionOf twin bounce ->
            computeApparentRoom twin
                |> Room.interpReflect bounce.axis 1

apparentHallway : ApparentRoom -> List Room.Model 
apparentHallway ar = 
    case ar of 
        ActualRoom room -> [ room ]
        ReflectionOf twin _ ->
            computeApparentRoom ar :: apparentHallway twin

lastApparentRoom : Room.Model -> List Sightray.MirrorBounce -> ApparentRoom
lastApparentRoom room bounces = 
    case bounces of 
        [] -> ActualRoom room
        b :: bs -> 
            ReflectionOf (lastApparentRoom room bs) b

bouncesToHallway room bounces = 
    lastApparentRoom room bounces
        |> apparentHallway



flipRoom : Sightray.MirrorBounce -> Room.Model -> Room.Model
flipRoom bounce =
    Room.interpReflect bounce.axis 1


reflectedRoom bounces room =
    case bounces of
        [] -> room
        b :: bs ->
            room 
                |> Room.interpReflect b.axis 1
                |> reflectedRoom bs
                
-- reflectedRooms : Model -> Shared.SuccessAnimation -> List Room.Model
-- reflectedRooms model animation =
--     rayNormal model
--         |> .bounces 
--         |> List.inits
--         |> List.map (\bounces -> reflectedRoom bounces model.room)


-- viewDiagramSuccess model animation = 
--     reflectionHallway model animation
--         |> List.map (\(room, ray) -> viewRoomWithRay room ray)
--         |> Svg.g []
--         |> Svg.at (Shared.pixelsPerMeter 0.3)--(0.6 ^ toFloat animation.step))
--         |> Svg.relativeTo Shared.topLeftFrame

-- reflectionHallway : Model -> Shared.SuccessAnimation -> Hallway
-- reflectionHallway model ani =


-- type alias Hallway = 
--     { closeRoom : Room
--     , farRoom : Room
--     , farRoomRay : Sightray
--     }

-- viewRoomWithRay : Room.Model -> Sightray -> Svg Msg
-- viewRoomWithRay room ray =
--     Svg.g [] [] -- TODO

rayNormal model =
    Sightray.fromRoomAndProjectedPath model.room.wallShape
        (Room.projectedSightline model.room)
    

viewRayNormal : Model -> Svg Msg
viewRayNormal model =
    rayNormal model
        |> Sightray.view

raySuccess : Model -> SuccessAnimation -> Sightray
raySuccess model animation = 
    let normal = rayNormal model in
    normal
        |> Sightray.unravel
        |> Array.fromList
        |> Array.get animation.step
        |> Maybe.andThen (Sightray.tail >> Maybe.map Tuple.second)
        |> Maybe.withDefault normal

viewRaySuccess : Model -> SuccessAnimation -> Svg Msg
viewRaySuccess model animation =
    let
        -- (successRay, centerPoint) = 
        --     rayNormal model
        --         |> Sightray.unravel
        --         |> Array.fromList
        --         |> (\rs -> Array.get animation.step rs)
        --         |> Maybe.map (\r -> 
        --             case Sightray.tail r of 
        --                 Nothing -> (r, Sightray.startPos r.start)
        --                 Just (bounce, tail) ->
        --                     ( tail 
        --                         -- |> Sightray.interpReflect bounce.axis 
        --                         --     (animation.transitionPct |> Maybe.withDefault 0)
        --                     , bounce.point
        --                     )
        --         )
        --             -- ( { r | start = Sightray.updateStartPos (\_ -> model.room.viewerPos) r.start }
        --             -- , Sightray.startPos r.start
        --             -- ))
        --         |> Maybe.withDefault (rayNormal model, model.room.viewerPos)  
        
        lineAttrs color = 
            [ Attr.fill "none" 
            , Attr.stroke color
            , Attr.strokeWidth "0.03"
            , Attr.strokeDasharray "0.05"
            ]

        raySucc = 
            raySuccess model animation 
    in
    raySucc
        |> Sightray.vertices
        |> Polyline2d.fromVertices
        |> (\poly -> Svg.g [] 
            [ Svg.polyline2d (lineAttrs "black") poly 
            , Svg.lineSegment2d (lineAttrs "red") 
                (LineSegment2d.from model.room.viewerPos (Sightray.startPos raySucc.start))
            ])
            
    

viewAnimationButtons : Model -> Element Msg 
viewAnimationButtons model = 
    El.row 
        [ El.centerX 
        , El.spacing 20 
        , Border.width (if Maybe.isJust model.successAnimation then 2 else 0)
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


debugLogF : (a -> b) -> String -> a -> a
debugLogF f str a =
    let _ = Debug.log str (f a) in 
    a