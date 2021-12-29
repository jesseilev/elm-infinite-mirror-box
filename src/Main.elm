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
    | PhotoSuccess Shared.SuccessAnimation

type FailReason
    = NoItem
    | WrongItem RoomItem
    | TooClose Length

init : () -> ( Model, Cmd Msg )
init _ =
    { room = Room.init1
    , sightDirection = Direction2d.fromAngle (Angle.degrees -15)
    , sightDistance = Length.meters 8.0
    , mouseDragPos = Nothing
    , dragging = False
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
                    Just pct -> pct + 0.05 |> (\newPct -> 
                        if newPct < 1.0 then 
                            { ani | transitionPct = Just newPct }
                        else 
                            { ani | transitionPct = Nothing, step = ani.step + 1 }
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
            itemHit == Just (Room.targetItem model.room)
                && closeEnough (Sightray.length ray) model.sightDistance

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

-- roomStatus model = 
--     case ((successAnimation model), model.dragging) of
--         (Just _, _) -> Room.TakingPic
--         (_, False) -> Room.Standing
--         (_, True) -> Room.LookingAround

-- updateRoomStatus model =
--     { model | room = Room.update (Room.setStatusMsg <| roomStatus model) model.room }

-- TODO just make it an actual Item in the first place
targetItem model =
    RoomItem.init model.room.targetPos RoomItem.emojis.parrot


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
        -- , Background.color (El.rgb 0.5 0.5 0.5)
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
                -- , Font.wordSpacing 4
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
                    -- , Background.color <| El.rgb 0.9 0.9 0.9
                    ] 
                    [ viewParagraph <| instructionsText model.sightDistance
                    , El.el 
                        [ El.centerX 
                        ] 
                        <| El.html (svgContainer model) 
                    , sightrayDistance model
                        |> String.fromFloat
                        |> (\dist -> "Your distance: " ++ dist ++ " meters")
                        |> El.text
                    , successAnimation model 
                        |> Debug.toString
                        |> El.text
                    ]
                ]
            )
        )

sightrayDistance model =
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
        [ Pointer.onDown (\_ -> 
            if Maybe.isJust (successAnimation model) then NoOp else DragStart)
        , Pointer.onUp (\_ -> 
            if Maybe.isJust (successAnimation model) then StepAnimation 1 else DragStop)
        , Pointer.onLeave (\_ -> DragStop)
        , Pointer.onMove (\event -> 
            if model.dragging then 
                MouseDragAt (mouseToSceneCoords (currentZoomScale model) event.pointer.offsetPos)
            else 
                NoOp)
        , Html.Attributes.style "cursor" (if model.dragging then "grabbing" else "grab")
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
    let 
        ray = rayNormal model 
    in
    Svg.g []
        [ if model.dragging then 
            angleArcs ray
                |> List.indexedMap (\i (wedge1, wedge2) -> 
                    [ viewWedge (angleColor i) wedge1, viewWedge (angleColor i) wedge2 ])
                |> List.concat
                |> Svg.g []
            else 
                Shared.svgEmpty
        , Sightray.view ray
        , Room.view model.room
        , model.mouseDragPos |> Maybe.map (debugCircle "red") |> Maybe.withDefault Shared.svgEmpty
        -- , rayEndpointLabel ray
        -- , Shared.viewFrame "red" 
        --     (Shared.playerFrame model.room.playerItem.pos (Direction2d.toAngle model.sightDirection))

        ]
        |> Svg.at (pixelsPerMeterWithZoomScale 1)
        |> Svg.relativeTo Shared.svgFrame

debugCircle color pos = 
    Svg.circle2d [ Attr.fill color ]
        (Circle2d.atPoint pos (Length.meters 0.125))

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

viewWedge color (angle, arc) =
    Svg.g 
        []
        [ 
        -- Svg.triangle2d [ Attr.fill "white" ] 
        --     (triangleForArc (arc |> Arc2d.scaleAbout (Arc2d.centerPoint arc) 3))
        -- ,
        Svg.arc2d 
            [ Attr.fill color
            , Attr.stroke "black" --fill
            , Attr.strokeWidth "0.005"
            -- , Attr.opacity "0.33"
            ] 
            arc
        , Svg.triangle2d [ Attr.fill color ] (triangleForArc arc)
        , Svg.text_ 
            [ Attr.fontSize "0.125" --(String.fromFloat "fontSize")
            , Attr.x (-0.5 * 0.25 |> String.fromFloat)
            , Attr.fill "black"
            -- , Attr.opacity "0"
            , Attr.alignmentBaseline "central"
            ] 
            [ angle 
                |> Quantity.abs 
                |> Angle.inDegrees 
                |> round 
                |> String.fromInt 
                |> (\s -> s ++ "º")
                |> Svg.text 
            ]
            |> Svg.mirrorAcross (Axis2d.through Point2d.origin Direction2d.x)
            |> Svg.translateBy (Vector2d.from Point2d.origin 
                (Arc2d.midpoint 
                    (arc |> Arc2d.scaleAbout (Arc2d.centerPoint arc) arcLabelDistanceScale)
                )
            )
        ]

angleColor i = 
    [ Shared.colors.yellow1 
    , Shared.colors.blue1 
    , Shared.colors.red1
    , Shared.colors.green1
    ] 
        |> Array.fromList 
        |> Array.get (i |> modBy 4)
        |> Maybe.map (\_ -> "white")
        |> Maybe.withDefault "grey"


angleArcRadius = 0.25
arcLabelDistanceScale = 1.65


triangleForArc arc = 
    Triangle2d.from (Arc2d.centerPoint arc)
        (Arc2d.startPoint arc) 
        (Arc2d.endPoint arc)

viewDiagramSuccess : Model -> Shared.SuccessAnimation -> Svg Msg
viewDiagramSuccess model animation = 
    let 
        (currentRay, nextRayM) = 
            rayNormal model |> (\ray ->
                ray 
                    |> Sightray.uncurledSeries
                    |> Array.fromList
                    |> (\rs -> 
                        ( Array.get animation.step rs |> Maybe.withDefault ray
                        , Array.get (animation.step + 1) rs
                        )
                    )
            )

        rooms = 
            currentRay |> Sightray.hallway model.room

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
                animation.transitionPct

        centroid = 
            transitionRoomM
                |> Maybe.map (\tr -> tr :: rooms)
                |> Maybe.withDefault rooms
                |> List.map (.wallShape >> Polygon2d.vertices)
                |> List.concat
                |> Polygon2d.convexHull
                |> Polygon2d.centroid
                |> Maybe.withDefault currentRay.startPos

        viewSuccessRay sr = 
            Svg.g []
                [ Sightray.view sr
                , Svg.lineSegment2d 
                    (Sightray.lineAttrsDefault ++ [ Attr.stroke Shared.colors.yellow1, Attr.strokeWidth "0.04" ])
                    (LineSegment2d.from model.room.playerItem.pos sr.startPos)
                ]

        transitionRayM = 
            Maybe.map2 (\nextRay pct -> Sightray.interpolateFrom currentRay nextRay pct)
                nextRayM
                animation.transitionPct

        -- transitionRay =
        --     Sightray.tail currentRay 
        --         |> Maybe.map (\(nextBounce, tail) -> 
        --             Sightray.interpReflect nextBounce.axis 
        --                 (animation.transitionPct |> Maybe.withDefault 0) 
        --                 tail
        --         )

    in
    Svg.g [ ] 
        [ transitionRayM |> Maybe.withDefault currentRay |> viewSuccessRay 
        , rooms
            |> List.map Room.view
            |> Svg.g [ Attr.opacity "0.5" ]
        , transitionRoomM 
            |> Maybe.map Room.view
            |> Maybe.withDefault Shared.svgEmpty
        , Room.view model.room
        ]
        |> Svg.translateBy (Vector2d.from centroid Point2d.origin)
        |> Svg.at (pixelsPerMeterWithZoomScale (currentZoomScale model))
        |> Svg.relativeTo Shared.svgFrame


currentZoomScale : Model -> Float
currentZoomScale model = 
    let 
        zoomScaleForStep s = toFloat (s + 1) ^ -0.7 
        animation = successAnimation model
        step = animation |> Maybe.map .step |> Maybe.withDefault 0
        transitionPct = animation |> Maybe.andThen .transitionPct |> Maybe.withDefault 0
    in
    Float.interpolateFrom (zoomScaleForStep step) 
        (zoomScaleForStep (step + 1))
        transitionPct 

angleArcs ray = 
    let 
        mkWedge bounce neighbor = 
            let 
                pointOnAxis modDirection = 
                    Point2d.translateIn (Axis2d.direction bounce.axis |> modDirection) 
                        (Length.meters angleArcRadius) 
                        bounce.point
            in 
            takeMin (Point2d.distanceFrom neighbor) 
                (pointOnAxis identity) 
                (pointOnAxis Direction2d.reverse) 
                |> (\poa -> 
                    Shared.angleDiff bounce.point poa neighbor
                        |> Maybe.withDefault (Angle.degrees 0)
                        |> within180
                        |> (\angle -> ( angle, Arc2d.sweptAround bounce.point angle poa ))
                )
                
    in
    Sightray.bouncesWithNeighborPoints ray 
        |> List.map (\(prev, bounce, next) -> ( mkWedge bounce prev, mkWedge bounce next )) 

within180 : Angle -> Angle 
within180 angle = 
    let 
        outOfRange = 
            (angle |> Quantity.greaterThan (Angle.degrees 180))
                || (angle |> Quantity.lessThan (Angle.degrees 180 |> Quantity.negate))

        fixPozi a = 
            if a |> Quantity.greaterThan (Angle.degrees 180) then 
                a |> Quantity.minus (Angle.degrees 360)
            else 
                a

        fixNegi a = 
            if a |> Quantity.lessThan (Angle.degrees 180 |> Quantity.negate) then 
                a |> Quantity.plus (Angle.degrees 360)
            else 
                a
    in
        angle 
            |> fixPozi
            |> fixNegi

takeMin : (a -> Quantity number c) -> a -> a -> a
takeMin quantify p q = 
    case Quantity.compare (quantify p) (quantify q) of
        GT -> q
        _ -> p



rayNormal : Model -> Sightray
rayNormal model =
    Sightray.fromRoomAndProjectedPath model.room
        (Shared.projectedSightline model.room.playerItem.pos 
            model.sightDirection 
            model.sightDistance
        )

-- Frame, Units, Conversions --

pixelsPerMeterWithZoomScale zoomScale = 
    Shared.pixelsPerMeter
        |> Quantity.multiplyBy zoomScale


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


