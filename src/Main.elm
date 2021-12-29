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
import Sightray exposing (startPos)
import Sightray exposing (Sightray)
import Sightray exposing (interpReflect)
import Circle2d exposing (centerPoint)
import TypedSvg.Types exposing (YesNo(..))
import Room exposing (projectedSightline)
import Arc2d
import TypedSvg.Attributes exposing (direction)
import RayPath exposing (nextBounce)

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
                    [ viewParagraph <| instructionsText model.room.sightDistance
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
                MouseDragAt (mouseToSceneCoords model.zoomScale event.pointer.offsetPos)
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
        , Room.view model.room |> Svg.map RoomMsg
        -- , rayEndpointLabel ray
        ]
        |> Svg.at (pixelsPerMeterWithZoomScale 1)
        |> Svg.relativeTo Shared.topLeftFrame

rayEndpointLabel ray = 
    let 
        itemM = 
            Sightray.endItem ray
        center = 
            itemM |> Maybe.map .pos |> Maybe.withDefault (Sightray.endPos ray.end)
            
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
                |> Maybe.withDefault (Sightray.startPos currentRay.start)

        viewSuccessRay sr = 
            Svg.g []
                [ Sightray.view sr
                , Svg.lineSegment2d 
                    (Sightray.lineAttrsDefault ++ [ Attr.stroke Shared.colors.yellow1, Attr.strokeWidth "0.04" ])
                    (LineSegment2d.from model.room.viewerPos (Sightray.startPos sr.start))
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

        zoomScaleForStep s = 
            toFloat (s + 1) ^ -0.7

        currentZoomScale = 
            Float.interpolateFrom (zoomScaleForStep animation.step) 
                (zoomScaleForStep (animation.step + 1))
                (animation.transitionPct |> Maybe.withDefault 0)
    in
    Svg.g [ ] 
        [ transitionRayM |> Maybe.withDefault currentRay |> viewSuccessRay 
        , rooms
            |> List.map (Room.view >> Svg.map RoomMsg)
            |> Svg.g [ Attr.opacity "0.5" ]
        , transitionRoomM 
            |> Maybe.map (Room.view >> Svg.map RoomMsg)
            |> Maybe.withDefault Shared.svgEmpty
        , Room.view model.room |> Svg.map RoomMsg
        ]
        |> Svg.translateBy (Vector2d.from centroid Point2d.origin)
        |> Svg.at (pixelsPerMeterWithZoomScale currentZoomScale)
        |> Svg.relativeTo Shared.topLeftFrame


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


refRooms : LineSegment -> Room.Model -> List (Sightray.MirrorBounce, Room.Model)
refRooms sightline room = 
    let
        nextRoom : 
            (LineSegment, Room.Model) 
            -> Maybe ((Sightray.MirrorBounce, Room.Model), (LineSegment, Room.Model))
        nextRoom (sl, r) = 
            case Sightray.nextIntersection r sl of 
                Just (Sightray.IntersectMirror bounce) ->
                    let nextR = r |> Room.interpReflect bounce.axis 1 in
                    Just 
                        ( (bounce, nextR)
                        , ( LineSegment2d.from bounce.point (LineSegment2d.endPoint sl)
                          , nextR
                          )
                        )
                _ ->
                    Nothing
    in
    List.unfoldr nextRoom (sightline, room)




type Chain item rel
    = Nil 
    | Cons item (Maybe (Chainlink rel item))

type Chainlink rel item 
    = Chainlink rel (Chain item rel)

type alias Hallway = Chain Room.Model Sightray.MirrorBounce


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



{-
[ ( Point2d { x = 0.35, y = -0.65 }
  , { axis = 
        Axis2d 
            { direction = 
                Direction2d { x = -0.9486832980505138, y = 0.3162277660168379 }
            , originPoint = 
                Point2d { x = 0.9116525411117986, y = 1.4461158196294002 } 
            }
    , point = Point2d { x = 0.9116525411117986, y = 1.4461158196294002 }
    , wall = 
        LineSegment2d 
            ( Point2d { x = 2.25, y = 1 }
            , Point2d { x = -1.5, y = 2.25 }
            ) 
    }
  , Point2d { x = -0.3010947202186196, y = -1.575273680054655 }
  )
, ( Point2d { x = 0.9116525411117986, y = 1.4461158196294002 }
  , { axis = 
        Axis2d 
            { direction = Direction2d { x = 0.9701425001453319, y = 0.24253562503633297 }
            , originPoint = Point2d { x = -0.3010947202186196, y = -1.575273680054655 } 
            }
    , point = Point2d { x = -0.3010947202186196, y = -1.575273680054655 }
    , wall = 
        LineSegment2d 
            ( Point2d { x = -2, y = -2 }
            , Point2d { x = 2, y = -1 }
            ) 
    }
  , Point2d { x = -1.8016028715674568, y = -0.31362440832338306 }
  )
, ( Point2d { x = -0.3010947202186196, y = -1.575273680054655 }
  , { axis = 
        Axis2d 
            { direction = Direction2d { x = -0.1168412475673972, y = -0.9931506043228762 }
            , originPoint = Point2d { x = -1.8016028715674568, y = -0.31362440832338306 } 
            }
    , point = Point2d { x = -1.8016028715674568, y = -0.31362440832338306 }
    , wall = 
        LineSegment2d 
            ( Point2d { x = -1.5, y = 2.25 }
            , Point2d { x = -2, y = -2 }
            ) 
    }
  , Point2d { x = -1.2529381897372662, y = -0.038419010331397996 }
  )
]
-}