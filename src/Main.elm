module Main exposing (..)

import Angle exposing (Angle)
import Arc2d
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
import Float.Extra as Float
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
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
import Triangle2d
import Sightray exposing (Sightray)
import Circle2d exposing (centerPoint)
import TypedSvg.Types exposing (YesNo(..))
import TypedSvg.Attributes exposing (direction)
import Ease

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
    , sightDirection : Direction
    , sightDistance : Length
    , mouseDragPos : Maybe Point
    , dragging : Bool
    , photoAttempt : Maybe PhotoAttempt
    }

type PhotoAttempt
    = PhotoFail FailReason
    | PhotoSuccess SuccessAnimation

type FailReason
    = NoItem
    | WrongItem RoomItem
    | TooClose Length

type alias SuccessAnimation = 
    { step : Int 
    , transitionPct : Maybe Float
    }

init : () -> ( Model, Cmd Msg )
init _ =
    { room = Room.init1
    , sightDirection = Direction2d.fromAngle (Angle.degrees 75)
    , sightDistance = Length.meters 8.0
    , mouseDragPos = Nothing
    , dragging = False
    , photoAttempt = Nothing
    }
        |> noCmds


 -- MODEL PROPERTIES --

successAnimation : Model -> Maybe SuccessAnimation
successAnimation model = 
    case model.photoAttempt of 
        Just (PhotoSuccess ani) -> Just ani
        _ -> Nothing

currentZoomScale : Model -> Float
currentZoomScale model = 
    let 
        zoomScaleForStep s = toFloat (s + 1) ^ -0.7 
        animation = successAnimation model
        step = animation |> Maybe.map .step |> Maybe.withDefault 0
        transitionPct = animation |> Maybe.andThen .transitionPct |> Maybe.withDefault 0 |> pctForRoom
    in
    Float.interpolateFrom (zoomScaleForStep step) 
        (zoomScaleForStep (step + 1))
        (transitionPct) 

rayNormal : Model -> Sightray
rayNormal model =
    Sightray.fromRoomAndProjectedPath model.room
        (Shared.projectedSightline model.room.playerItem.pos 
            model.sightDirection 
            model.sightDistance
        )


rayDistance model =
    rayNormal model 
        |> Sightray.length
        |> (\dist -> 
            if closeEnough model.sightDistance dist then model.sightDistance else dist
        )
        |> Length.inMeters
        |> ((*) 100)
        |> round
        |> toFloat 
        |> (\l -> l / 100)


-- UPDATE --

type Msg 
    = NoOp
    | MouseDragAt Point
    | DragStart
    | DragStop
    | MouseClickAt Point
    | AdjustZoom Float
    | StepAnimation Int
    | Tick

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        MouseDragAt mousePosInScene ->
            if not model.dragging then 
                model |> noCmds 
            else 
                model.mouseDragPos 
                    |> Maybe.map (updatePlayerDirection model mousePosInScene)
                    |> Maybe.withDefault model
                    |> (\updModel -> 
                        { updModel | mouseDragPos = Just mousePosInScene, dragging = True }
                    )
                    |> noCmds

        DragStart -> 
            { model | dragging = True, mouseDragPos = Nothing }
                -- |> updateRoomStatus
                |> noCmds

        DragStop -> 
            { model | dragging = False, mouseDragPos = Nothing }
                |> updatePhotoAttempt
                -- |> updateRoomStatus
                |> noCmds

        MouseClickAt sceneOffsetPos -> 
            model 
                |> updatePhotoAttempt
                |> noCmds

        StepAnimation stepDiff ->
            model |> updateSuccessAnimation (\ani -> 
                { ani | transitionPct = Just 0.0 }
            )
                |> noCmds

        Tick -> 
            model |> updateSuccessAnimation (\ani -> 
                case ani.transitionPct of 
                    Nothing -> ani
                    Just pct -> pct + 0.01 |> (\newPct -> 
                        if newPct < 1.0 then 
                            { ani | transitionPct = Just newPct }
                        else 
                            { ani | step = ani.step + 1, transitionPct = Just 0 }
                        )
            )
                |> noCmds

        _ ->
            model |> noCmds


updatePlayerDirection : Model -> Point -> Point -> Model
updatePlayerDirection model mousePos prevMousePos =
    let playerAngle = Direction2d.toAngle model.sightDirection in
    Shared.angleDiff model.room.playerItem.pos prevMousePos mousePos
        |> Maybe.map (Quantity.plus playerAngle)
        |> Maybe.withDefault playerAngle
        |> (\angle -> { model | sightDirection = Direction2d.fromAngle angle })

updatePhotoAttempt : Model -> Model 
updatePhotoAttempt model = 
    let 
        ray = 
            rayNormal model

        sightEnd = 
            ray |> Sightray.endPos 

        itemHit = 
            Room.allItems model.room
                |> List.filter (RoomItem.containsPoint sightEnd)
                |> List.head -- TODO handle more than one? shouldnt be possible

        targetHit = 
            Sightray.endItem ray == Just (Room.targetItem model.room)
                && closeEnough (Sightray.length ray) model.sightDistance

        newPhotoAttempt = 
            case (model.photoAttempt, targetHit) of 
                (Nothing, True) -> 
                    Just <| PhotoSuccess (SuccessAnimation 0 (Just 0))
                -- (Nothing, Just False) ->
                --     Just <| PhotoFail (WrongItem itemHit)
                -- (Nothing, Nothing) ->
                --     Just <| PhotoFail (NoItem)
                --     -- TODO handle other fail types
                _ ->
                    model.photoAttempt
    in
    { model | photoAttempt = newPhotoAttempt }
    
updateSuccessAnimation : (SuccessAnimation -> SuccessAnimation) -> Model -> Model
updateSuccessAnimation upd model = 
    case model.photoAttempt of 
        Just (PhotoSuccess ani) -> { model | photoAttempt = Just (PhotoSuccess (upd ani)) }
        _ -> model

closeEnough : Length -> Length -> Bool
closeEnough =
    Quantity.equalWithin (RoomItem.radius |> Quantity.multiplyBy 2)

-- roomStatus model = 
--     case ((successAnimation model), model.dragging) of
--         (Just _, _) -> Room.TakingPic
--         (_, False) -> Room.Standing
--         (_, True) -> Room.LookingAround

-- updateRoomStatus model =
--     { model | room = Room.update (Room.setStatusMsg <| roomStatus model) model.room }


-- SUBSCRIPTIONS --

subscriptions : Model -> Sub Msg
subscriptions model = 
    successAnimation model
        |> Maybe.andThen .transitionPct
        |> Maybe.map (\_ -> Time.every 50 (\_ -> Tick))
        |> Maybe.withDefault Sub.none


-- VIEW --

view : Model -> Html Msg
view model = 
    El.layout 
        [ El.width El.fill
        , El.height El.fill 
        ]
        (El.el 
            [] 
            (El.column 
                [ El.centerX 
                , El.paddingXY 200 10
                , El.spacing 40
                -- , El.width <| El.px 800
                , Font.size 17
                , Font.family 
                    [ Font.external
                        { name = "Cantarell"
                        , url = "https://fonts.googleapis.com/css?family=Cantarell"
                        }
                    ]
                , Font.color (El.rgb 0.35 0.35 0.35)
                ]
                [ El.el [ Region.heading 1, Font.size 30 ] <| El.text "Pat's Infinite Bird Box"
                , viewParagraph patTheCatText
                , viewParagraph birdBoxText
                , El.column     
                    [ El.centerX 
                    , El.width El.fill
                    , El.padding 40
                    , El.spacing 20
                    , Border.width 2
                    , Border.color <| El.rgb 0.9 0.9 0.9
                    ] 
                    [ viewParagraph <| instructionsText model.sightDistance
                    , El.el 
                        [ El.centerX 
                        ] 
                        <| El.html (svgContainer model) 
                    , rayDistance model
                        |> String.fromFloat
                        |> (\dist -> "Your distance: " ++ dist ++ " meters")
                        |> El.text
                    -- , successAnimation model 
                    --     |> El.text
                    ]
                ]
            )
        )

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
    a few potted plants 🪴. But the light bouncing around off the mirrored walls gives 
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
        [ Pointer.onUp (\_ -> 
            if Maybe.isJust (successAnimation model) then StepAnimation 1 else DragStop)
        , Pointer.onLeave (\_ -> DragStop)
        , Pointer.onMove (\event -> 
            if model.dragging then 
                MouseDragAt (mouseToSceneCoords (currentZoomScale model) event.pointer.offsetPos)
            else 
                NoOp)
        -- , Html.Attributes.style "cursor" (if model.dragging then "grabbing" else "grab")
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
        [ viewBeam (successAnimation model) model
        , Room.view (currentZoomScale model) model.room
        ]
        |> Svg.at (pixelsPerMeterWithZoomScale 1)
        |> Svg.relativeTo Shared.svgFrame


rayPairForAnimation : SuccessAnimation -> Sightray -> (Sightray, Maybe Sightray)
rayPairForAnimation animation ray = 
    ray 
        |> Sightray.uncurledSeries
        |> Array.fromList
        |> (\rs -> 
            ( Array.get animation.step rs |> Maybe.withDefault ray
            , Array.get (animation.step + 1) rs
            )
        )

viewDiagramSuccess : Model -> SuccessAnimation -> Svg Msg
viewDiagramSuccess model animation = 
    let 
        initialRay = 
            rayNormal model

        (currentRay, nextRayM) = 
            transitionRay animation (rayNormal model)

        rooms = 
            currentRay |> Sightray.hallway model.room 
                |> Shared.debugLogF "rooms length" List.length

        farthestRoom = 
            rooms |> List.last |> Maybe.withDefault model.room

        nextRoomM = 
            currentRay
                |> Sightray.uncurl 
                |> Maybe.map (Sightray.hallway model.room)
                |> Maybe.andThen List.last

        transitionRoomM = 
            Maybe.map2 (Room.interpolateFrom farthestRoom)
                nextRoomM
                (animation.transitionPct |> Maybe.map pctForRoom)

        centroid = 
            rooms ++ (transitionRoomM |> Maybe.toList)
                |> List.map (.wallShape >> Polygon2d.vertices)
                |> List.concat
                |> Polygon2d.convexHull
                |> Polygon2d.centroid
                |> Maybe.withDefault initialRay.startPos
    in
    Svg.g [ ] 
        [ viewBeam (Just animation) model
        , rooms ++ (transitionRoomM |> Maybe.toList)
            |> List.map (Room.view (currentZoomScale model))
            |> Svg.g [ Attr.opacity "0.4" ]
        , Room.view (currentZoomScale model) model.room
        ]
        |> Svg.translateBy (Vector2d.from centroid Point2d.origin)
        |> Svg.at (pixelsPerMeterWithZoomScale (currentZoomScale model))
        |> Svg.relativeTo Shared.svgFrame

pctForRoom = 
    clamp 0 0.5 >> (\n -> n * 2) >> Ease.inOutQuad

pctForRay = 
    clamp 0.5 1 >> (\x -> (x * 2) - 1) >> Ease.inOutQuad

viewBeam : Maybe SuccessAnimation -> Model -> Svg Msg 
viewBeam animationM model = 
    let 
        ray = 
            rayNormal model

        options showAngles attrs = 
            { angleLabels = if showAngles then Sightray.AllAngles else Sightray.NoAngles 
            , attributes = attrs
            , projectionAttributes = []
            , zoomScale = currentZoomScale model
            }

        gAttrs = 
            if Maybe.isNothing animationM then 
                [ Attr.cursor (if model.dragging then "grabbing" else "grab") 
                , Pointer.onDown (\_ -> if Maybe.isNothing animationM then DragStart else NoOp)
                ]
            else
                []

        transitionIfAnimating beamRay = 
            animationM 
                |> Maybe.map (\ani -> transitionRay ani beamRay)
                |> Maybe.map (\(r, rM) -> Maybe.withDefault r rM)
                |> Maybe.withDefault beamRay
    in 
    Svg.g gAttrs
        (List.concat
            [ [ ray -- draw a thick invisible ray behind the others to detect the cursor consistently 
                |> Sightray.viewWithOptions (options False 
                    [ Attr.opacity "0", Attr.strokeDasharray "0", Attr.strokeWidth "0.25"]
                )
              ]
            , Sightray.beam model.room (projectedSightline model) -- the yellow beam rays
                |> List.map transitionIfAnimating
                |> List.indexedMap (\i -> 
                    Sightray.viewWithOptions (options False 
                        [ Attr.stroke Shared.colors.yellow1 
                        , Attr.strokeDashoffset <| Shared.floatAttribute 1 (toFloat i * 0.1)
                        , Attr.strokeDasharray <| Shared.floatAttribute 1 
                            (i |> modBy 3 |> toFloat |> ((*) 0.03) |> ((+) 0.05))
                        , Attr.strokeWidth <| Shared.floatAttribute (currentZoomScale model) 
                            (0.005 * (if model.dragging then 2 else 1))
                        ]
                    )
                )
            , [ ray -- the "true" ray, invisible but showing angles during mouse drag
                |> Sightray.viewWithOptions (options model.dragging [ Attr.opacity "0"]) 
              ]
            ]
        )

transitionRay : SuccessAnimation -> Sightray -> (Sightray, Maybe Sightray)
transitionRay animation curledRay = 
    rayPairForAnimation animation curledRay 
        |> (\(uncurledRay, nextUncurledRayM) -> 
            ( uncurledRay
            , Maybe.map2 (Sightray.interpolateFrom uncurledRay) 
                nextUncurledRayM 
                (animation.transitionPct |> Maybe.map pctForRay)
            )
        )

projectedSightline model = 
    Shared.projectedSightline model.room.playerItem.pos model.sightDirection model.sightDistance

rayEndpointLabel ray = 
    let 
        itemM = 
            Sightray.endItem ray
        center = 
            itemM |> Maybe.map .pos |> Maybe.withDefault (Sightray.endPos ray)
            
    in
    Svg.g [] 
        [ Svg.circle2d 
            [ Attr.fill <| if Maybe.isJust itemM then "none" else "white"
            , Attr.strokeWidth "0.01"
            , Attr.stroke "black"
            ]
            (Circle2d.atPoint center RoomItem.radius)
        , Svg.text_ 
            [ Attr.fontSize "0.125" --(String.fromFloat "fontSize")
            , Attr.x (-0.5 * 0.25 |> String.fromFloat)
            , Attr.fill "black"
            , Attr.alignmentBaseline "central"
            ] 
            [ "8m"
                |> Svg.text 
            ]
            |> Svg.mirrorAcross (Axis2d.through Point2d.origin Direction2d.x)
            |> Svg.translateBy (Vector2d.from Point2d.origin center)
        ]

-- Frame, Units, Conversions --

pixelsPerMeterWithZoomScale : Float -> Quantity Float (Quantity.Rate Pixels Meters)
pixelsPerMeterWithZoomScale zoomScale = 
    Shared.pixelsPerMeter |> Quantity.multiplyBy zoomScale

mouseToSceneCoords : Float -> (Float, Float) -> Point2d Meters SceneCoords
mouseToSceneCoords zoomScale (x, y) = 
    Point2d.pixels x y
        |> Point2d.placeIn Shared.svgFrame
        |> Point2d.at_ (pixelsPerMeterWithZoomScale zoomScale)

svgToSceneCoords : Frame2d Pixels globalC { defines : localC } -> Svg msg -> Svg msg
svgToSceneCoords localFrame svg =
    svg 
        |> Svg.mirrorAcross Axis2d.x 
        |> Svg.placeIn localFrame


