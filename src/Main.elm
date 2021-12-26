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
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
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
import RoomItem exposing (RoomItem)
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
    , dragging : Bool
    , zoomScale : Float
    , photoAttempt : Maybe PhotoAttempt
    }

type PhotoAttempt
    = PhotoFail FailReason
    | PhotoSuccess Shared.SuccessAnimation

type FailReason
    = NoItem
    | WrongItem RoomItem
    | TooClose Length

init : () -> ( Model, Cmd Msg )
init _ =
    { room = Room.init1
    , mouseDragPos = Nothing
    , dragging = False
    , zoomScale = 0.3
    , photoAttempt = Nothing
    }
        |> noCmds

successAnimation : Model -> Maybe Shared.SuccessAnimation
successAnimation model = 
    case model.photoAttempt of 
        Just (PhotoSuccess ani) -> Just ani
        _ -> Nothing


-- UPDATE --

type Msg 
    = NoOp
    | MouseDragAt (Point2d Meters SceneCoords)
    | DragStart
    | DragStop
    | MouseClickAt (Point2d Meters SceneCoords)
    | AdjustZoom Float
    | RoomMsg Room.Msg
    | StepAnimation Int
    | Tick

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        MouseDragAt mousePosInScene ->
            if not model.dragging then 
                model |> noCmds 
            else 
                { model 
                    | room = 
                        model.mouseDragPos
                            |> Maybe.map (\prevMouse -> 
                                Room.update (Room.mouseDragMsg prevMouse mousePosInScene) model.room)
                            |> Maybe.withDefault model.room
                    , mouseDragPos = Just mousePosInScene
                    , dragging = True
                }
                    |> noCmds

        DragStart -> 
            { model | dragging = True, mouseDragPos = Nothing }
                |> updateRoomStatus
                |> noCmds

        DragStop -> 
            { model | dragging = False, mouseDragPos = Nothing }
                |> updatePhotoAttempt
                |> updateRoomStatus
                |> noCmds

        MouseClickAt sceneOffsetPos -> 
            model 
                |> updatePhotoAttempt
                |> noCmds
        
        AdjustZoom deltaY ->
            { model  
                | zoomScale = model.zoomScale * (1.1 ^ (sign deltaY))
            }
                |> noCmds

        StepAnimation stepDiff ->
            model |> updateSuccessAnimation (\ani -> 
                { ani | step = ani.step + 1, transitionPct = Just 0.3 }
            )
                |> noCmds

        Tick -> 
            model |> updateSuccessAnimation (\ani -> 
                case ani.transitionPct of 
                    Nothing -> ani
                    Just pct -> pct + 0.02 |> (\newPct -> 
                        if newPct < 1.0 then 
                            { ani | transitionPct = Just newPct }
                        else 
                            { ani | transitionPct = Nothing, step = ani.step }
                        )
            )
                |> noCmds

        _ ->
            model |> noCmds

updatePhotoAttempt : Model -> Model 
updatePhotoAttempt model = 
    let 
        ray = 
            rayNormal model

        sightEnd = 
            ray |> .end |> Sightray.endPos 

        itemHit = 
            Room.allItems model.room
                |> List.filter (RoomItem.containsPoint sightEnd)
                |> List.head -- TODO handle more than one? shouldnt be possible

        targetHit = 
            itemHit == Just (targetItem model)
                && closeEnough (Sightray.length ray) model.room.sightDistance

        newPhotoAttempt = 
            case (model.photoAttempt, targetHit) of 
                (Nothing, True) -> 
                    Just <| PhotoSuccess (Shared.SuccessAnimation 0 Nothing)
                -- (Nothing, Just False) ->
                --     Just <| PhotoFail (WrongItem itemHit)
                -- (Nothing, Nothing) ->
                --     Just <| PhotoFail (NoItem)
                --     -- TODO handle other fail types
                _ ->
                    model.photoAttempt
    in
    { model | photoAttempt = newPhotoAttempt }

updateSuccessAnimation : (Shared.SuccessAnimation -> Shared.SuccessAnimation) -> Model -> Model
updateSuccessAnimation upd model = 
    case model.photoAttempt of 
        Just (PhotoSuccess ani) -> { model | photoAttempt = Just (PhotoSuccess (upd ani)) }
        _ -> model

roomStatus model = 
    case ((successAnimation model), model.dragging) of
        (Just _, _) -> Room.TakingPic
        (_, False) -> Room.Standing
        (_, True) -> Room.LookingAround

updateRoomStatus model =
    { model | room = Room.update (Room.setStatusMsg <| roomStatus model) model.room }

-- TODO just make it an actual Item in the first place
targetItem model =
    RoomItem.init model.room.targetPos RoomItem.emojis.parrot


-- SUBSCRIPTIONS --

subscriptions : Model -> Sub Msg
subscriptions model = 
    case (successAnimation model) |> Maybe.map .transitionPct of
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
        -- , Background.color Shared.colors.darkBackground
        ]
        (El.el 
            [ El.centerX
            , El.centerY 
            ] 
            (El.column 
                [ El.centerX 
                , El.paddingXY 100 10
                , El.spacing 40
                , Font.size 17
                , Font.family 
                    [ Font.external
                        { name = "Cantarell"
                        , url = "https://fonts.googleapis.com/css?family=Cantarell"
                        }
                    ]
                -- , Font.wordSpacing 4
                , Font.color (El.rgb 0.35 0.35 0.35)
                ]
                [ El.el [ Region.heading 1, Font.size 30 ] <| El.text "Pat's Infinite Bird Box"
                , viewParagraph patTheCatText
                , viewParagraph birdBoxText
                , El.column     
                    [ El.centerX 
                    , El.padding 40
                    , El.spacing 20
                    , Border.width 2
                    , Border.color <| El.rgb 0.9 0.9 0.9
                    -- , Background.color <| El.rgb 0.9 0.9 0.9
                    ] 
                    [ viewParagraph <| instructionsText model.room.sightDistance
                    , El.el 
                        [ El.centerX 
                        ] 
                        <| El.html (svgContainer model) 
                    , rayNormal model 
                        |> Sightray.length
                        |> (\dist -> 
                            if closeEnough model.room.sightDistance dist 
                                then
                                    model.room.sightDistance 
                                else 
                                    dist
                        )
                        |> Length.inMeters
                        |> ((*) 100)
                        |> round
                        |> toFloat 
                        |> (\l -> l / 100)
                        |> String.fromFloat
                        |> (\dist -> "Your distance: " ++ dist ++ " meters")
                        |> El.text
                    ]
                ]
            )
        )

closeEnough =
    Quantity.equalWithin (RoomItem.radius |> Quantity.multiplyBy 2)

viewParagraph text = 
    El.paragraph 
        [ El.paddingXY 0 0, El.spacing 12 ] 
        [ El.text text ]


patTheCatText = 
    """Pat the Cat 🐈‍⬛ is an avid wildlife photographer. 
    She recently bought a fancy new camera 📷, and is excited to test out its zoom 
    abilities."""

birdBoxText = 
    """Arriving home, Pat enters her Infinite Bird Box, a small room with 4 
    adjustable mirrors for walls. The room doesn't contain much, just Garrett the Parrot 🦜 and 
    a few potted plants 🪴. But the way the light bounces around off the mirrored walls gives 
    Pat the illusion of standing in a vast forest surrounded by many plants and birds:
    some close by, and others far away..."""

instructionsText sightDistance = 
    "Your challenge: Click and drag to aim Pat's camera "
    ++ "at the reflected image of a bird that appears to be " 
    ++ String.fromFloat (Quantity.unwrap sightDistance)
    ++ " meters away."


svgContainer : Model -> Html Msg
svgContainer model =
    Html.div 
        [ Pointer.onDown (\_ -> 
            if Maybe.isJust (successAnimation model) then NoOp else DragStart)
        , Pointer.onUp (\_ -> 
            if Maybe.isJust (successAnimation model) then StepAnimation 1 else DragStop)
        , Pointer.onLeave (\_ -> DragStop)
        , Pointer.onMove (\event -> 
            if model.dragging then 
                MouseDragAt (mouseToSceneCoords model.zoomScale event.pointer.offsetPos)
            else 
                NoOp)
        -- , Wheel.onWheel (\event -> AdjustZoom event.deltaY)
        -- , Mouse.onClick (\event -> if model.dragging then NoOp else MouseClickAt Point2d.origin)
        ]
        [ Svg.svg 
            [ Attr.width (constants.containerWidth |> String.fromFloat)
            , Attr.height (constants.containerHeight |> String.fromFloat)
            ]
            [ Svg.g [] 
                [ (successAnimation model) 
                    |> Maybe.map (viewDiagramSuccess model)
                    |> Maybe.withDefault (viewDiagramNormal model)
                ]
            ]
        ]

viewDiagramNormal : Model -> Svg Msg
viewDiagramNormal model =
    Svg.g []
        [ Sightray.view (rayNormal model)
        , Room.view model.room |> Svg.map RoomMsg
        ]
        |> Svg.at (pixelsPerMeterWithZoomScale 1)
        |> Svg.relativeTo Shared.topLeftFrame
        
viewDiagramSuccess : Model -> Shared.SuccessAnimation -> Svg Msg
viewDiagramSuccess model animation = 
    let 
        ray = raySuccess model animation 

        rooms =
            reflectedRooms (projectedSightline model.room) model.room []
                |> List.reverse
                |> List.take (animation.step + 1)

        centroid = 
            rooms 
                |> List.map (.wallShape >> Polygon2d.vertices)
                |> List.concat
                |> Polygon2d.convexHull
                |> Polygon2d.centroid
                |> Maybe.withDefault (Sightray.startPos ray.start)

        successAttrs = 
            Sightray.lineAttrs "lightGreen" "0.05"
    in
    Svg.g [ ] 
        [ Sightray.viewWithAttrs successAttrs ray
        , Svg.lineSegment2d successAttrs
            (LineSegment2d.from model.room.viewerPos (Sightray.startPos ray.start))
        , rooms
            |> List.map (Room.view >> Svg.map RoomMsg)
            |> Svg.g [ Attr.opacity "0.5" ]
        , Room.view model.room |> Svg.map RoomMsg
        ]
        |> Svg.translateBy (Vector2d.from centroid Point2d.origin)
        |> Svg.at (pixelsPerMeterWithZoomScale (toFloat (animation.step + 1) ^ -0.7))
        |> Svg.relativeTo Shared.topLeftFrame

reflectedRooms : LineSegment -> Room.Model -> List Room.Model -> List Room.Model
reflectedRooms sightline room roomsAcc = 
    case Sightray.nextIntersection room sightline of
        Just (Sightray.IntersectMirror bounce) -> 
            reflectedRooms 
                (LineSegment2d.from bounce.point (LineSegment2d.endPoint sightline))
                (room |> Room.interpReflect bounce.axis 1)
                (room :: roomsAcc)
        _ -> 
            room :: roomsAcc

rayNormal : Model -> Sightray
rayNormal model =
    Sightray.fromRoomAndProjectedPath model.room
        (Room.projectedSightline model.room)

raySuccess : Model -> SuccessAnimation -> Sightray
raySuccess model animation = 
    let normal = rayNormal model in
    normal
        |> Sightray.unravel
        |> Array.fromList
        |> Array.get animation.step
        -- |> Maybe.andThen (Sightray.tail >> Maybe.map Tuple.second)
        |> Maybe.withDefault normal
            
viewAnimationButtons : Model -> Element Msg 
viewAnimationButtons model = 
    (successAnimation model)
        |> Maybe.map (\_ ->
            El.row 
                [ El.centerX 
                , El.spacing 20 
                ]
                [ Input.button []
                    { onPress = Just (StepAnimation 1)
                    , label = El.text ">"
                    }
                ])
        |> Maybe.withDefault El.none

-- Frame, Units, Conversions --

pixelsPerMeterWithZoomScale zoomScale = 
    Shared.pixelsPerMeter
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
        |> Point2d.at_ (pixelsPerMeterWithZoomScale zoomScale)

svgToSceneCoords : Frame2d Pixels globalC { defines : localC } -> Svg msg -> Svg msg
svgToSceneCoords localFrame svg =
    svg 
        |> Svg.mirrorAcross Axis2d.x 
        |> Svg.placeIn localFrame

