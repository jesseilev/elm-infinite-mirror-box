module Playground1 exposing (..)

import Angle exposing (Angle)
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
import Maybe.Extra as Maybe
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Vector2d exposing (Vector2d)
import Convert exposing (lengthToPixels)
import Rectangle2d exposing (Rectangle2d)
import SketchPlane3d exposing (toPlane)
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Types exposing (CoordinateSystem(..), Paint(..))
main = 
    Browser.element
        { init = init
        , update = update   
        , subscriptions = subscriptions
        , view = view
        }


-- TYPES --

{- origin point in the center of the frame, x -> right, y -> up -}
type SceneCoords = SceneCoords

{- origin point at the top left of the frame, x -> right, y -> down -}
type TopLeftCoords = TopLeftCoords 

type alias Point = Point2d Meters SceneCoords
type alias LineSegment = LineSegment2d Meters SceneCoords
type alias Polygon = Polygon2d Meters SceneCoords
type alias Polyline = Polyline2d Meters SceneCoords
type alias Axis = Axis2d Meters SceneCoords


-- MODEL --

type alias Model = 
    { roomShape : Polygon
    , viewerPos : Point
    , viewerAngle : Angle
    , sightDistance : Quantity Float Meters
    , targetPos : Point
    , mouseDragPos : Maybe Point
    , mouseDown : Bool
    , clickPosDebug : Point
    , zoomScale : Float
    }

init : () -> ( Model, Cmd Msg )
init _ =
    { roomShape = 
        Polygon2d.singleLoop 
            [ Point2d.meters -2.0 -2.0
            , Point2d.meters 2.0 -1.0
            , Point2d.meters 2.25 1.0
            , Point2d.meters -1.5 2.25
            ]
    , viewerPos = Point2d.meters 0.3 -0.7
    , viewerAngle = Angle.degrees 50
    , sightDistance = Length.meters 8.0
    , targetPos = Point2d.meters 1.5 0.2
    , mouseDragPos = Nothing
    , mouseDown = False
    , clickPosDebug = Point2d.origin
    , zoomScale = 1
    }
        |> noCmds


-- UPDATE --

type Msg 
    = NoOp
    | MouseDragAt (Point2d Meters SceneCoords)
    | ToggleMouseDown Bool
    | MouseClickAt (Point2d Meters SceneCoords)
    | AdjustZoom Float

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        MouseDragAt mousePosInScene ->
            if not model.mouseDown then 
                model |> noCmds 
            else 
                { model 
                    | viewerAngle = 
                        model.mouseDragPos
                            |> Maybe.andThen 
                                (angleDiff (Frame2d.originPoint (viewerFrame model)) mousePosInScene)
                            |> Maybe.map (Quantity.plus model.viewerAngle)
                            |> Maybe.withDefault model.viewerAngle
                    , mouseDragPos = Just mousePosInScene
                    , mouseDown = True
                }
                    |> noCmds

        ToggleMouseDown isDown -> 
            { model | mouseDown = isDown, mouseDragPos = Nothing }
                |> noCmds

        MouseClickAt sceneOffsetPos -> 
            { model 
                | viewerPos = sceneOffsetPos |> Point2d.relativeTo (roomFrame model)
                , clickPosDebug = sceneOffsetPos
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

constants =
    { containerWidth = 500 
    , containerHeight = 500 
    }

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
        ]
        [ Svg.svg 
            [ Attr.width (constants.containerWidth |> String.fromFloat)
            , Attr.height (constants.containerHeight |> String.fromFloat)
            ]
            [ viewScene model ]
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

viewScene : Model -> Svg Msg 
viewScene model = 
    Svg.g [] 
        [ viewReflectedRooms model
        , Svg.polygon2d 
            [ Attr.fill "none"
            , Attr.strokeWidth "0.02"
            , Attr.stroke "black"
            ]
            (model.roomShape |> Polygon2d.placeIn (roomFrame model))
        , bouncePath model 
            |> Svg.polyline2d
                [ Attr.fill "none"
                , Attr.stroke "orange"
                , Attr.strokeWidth "0.02"
                , Attr.strokeDasharray "0.1"
                ]
        , roomItem model emojis.roundTree (Point2d.meters -0.5 0.3)
        , roomItem model emojis.roundTree (Point2d.meters 0.2 0.9)
        , roomItem model emojis.pineTree (Point2d.meters 1.1 -0.4)
        , roomItem model emojis.cat model.viewerPos
        , roomItem model emojis.parrot model.targetPos
        -- , viewDebugStuff model
        ]
        |> Svg.at (pixelsPerMeter model)
        |> Svg.relativeTo (topLeftFrame model)

emojis = 
    { roundTree = "🌳"
    , pineTree = "🌲"
    , palmTree = "🌴"
    , cat = "🐈‍⬛"
    , parrot = "🦜"
    }

viewReflectedRooms : Model -> Svg Msg 
viewReflectedRooms model = 
    let
        reflectedRoom mirrorWall = 
            model.roomShape 
                |> Polygon2d.mirrorAcross mirrorWall.axis
        
        wallM = 
            Polygon2d.edges model.roomShape
                |> List.head
                |> Maybe.andThen mkWall

        mkWall : LineSegment2d Meters SceneCoords -> Maybe { wall : LineSegment2d Meters SceneCoords, axis : Axis2d Meters SceneCoords }
        mkWall line = 
            LineSegment2d.direction line
                |> Maybe.map (Axis2d.through (LineSegment2d.startPoint line))
                |> Maybe.map (\a -> { wall = line, axis = a})
    in
        reflectedRooms (projectedSightline model) model.roomShape []
            |> List.map (Svg.polygon2d 
                [ Attr.fill "none"
                , Attr.stroke "grey"
                , Attr.strokeWidth "0.02" 
                ])
            |> Svg.g [] 

viewDebugStuff : Model -> Svg Msg 
viewDebugStuff model = 
    Svg.g []
        [ frameDebugViz "purple"
            |> Svg.placeIn (roomFrame model)
        , frameDebugViz "orange" 
            |> Svg.placeIn (viewerFrame model)
        -- , frameDebugViz "grey"
        --     |> Svg.relativeTo ((topLeftFrame model) |> Frame2d.translateBy (Vector2d.pixels -100 -100))
        , Svg.circle2d
            [ Attr.fill "red" ]
            (Circle2d.atPoint model.clickPosDebug (Length.meters 0.05))
        ]

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

viewerFrame : Model -> Frame2d Meters SceneCoords { defines : SceneCoords }
viewerFrame model = 
    roomFrame model
        |> Frame2d.rotateAround Point2d.origin model.viewerAngle
        |> Frame2d.translateBy (Vector2d.from Point2d.origin model.viewerPos)
        |> Frame2d.relativeTo (roomFrame model)

roomFrame : Model -> Frame2d Meters SceneCoords { defines : SceneCoords }
roomFrame model = 
    Frame2d.atOrigin
    -- viewerFrame model
        -- |> Frame2d.translateBy (Vector2d.reverse 
        --     (Vector2d.from Point2d.origin model.viewerPos))
        -- |> Frame2d.rotateAround (Frame2d.originPoint <| viewerFrame model) 
        --     (Quantity.negate model.viewerAngle)

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

bouncePath : Model -> Polyline
bouncePath model = 
    let 
        sightline = 
            projectedSightline model

        recurseModel bounce = 
            { model 
                | viewerPos = bounce.point
                , sightDistance = 
                    model.sightDistance 
                        |> Quantity.minus (Point2d.distanceFrom bounce.point model.viewerPos)
                , roomShape = 
                    model.roomShape
                        |> Polygon2d.mirrorAcross bounce.axis
            }

        vertices = 
            findIntersection sightline model.roomShape 
                |> Maybe.map (\inter -> 
                    recurseModel inter
                        |> bouncePath
                        |> Polyline2d.mirrorAcross inter.axis
                        |> Polyline2d.vertices
                        |> (\vs -> model.viewerPos :: vs))
                |> Maybe.withDefault [ model.viewerPos, (LineSegment2d.endPoint sightline) ]
    in
        Polyline2d.fromVertices vertices


maybePair : Maybe a -> Maybe b -> Maybe (a, b)
maybePair ma mb = 
    case (ma, mb) of 
        (Just x, Just y) -> Just (x, y)
        _ -> Nothing

sign : number -> number
sign n = 
    if n < 0 then -1 else 1


-- COMPUTING STUFF --

projectedSightline : Model -> LineSegment2d Meters SceneCoords
projectedSightline model =
    LineSegment2d.fromPointAndVector model.viewerPos
        (Vector2d.withLength model.sightDistance 
            (Frame2d.yDirection (viewerFrame model)))
        

findIntersection : LineSegment2d Meters SceneCoords -> Polygon2d Meters SceneCoords -> Maybe Intersection
findIntersection sightline roomShape =
    let
        -- "trim" off the very beginning of the sightline by shrinking it slightly
        -- we dont want to count the start point as an intersection
        -- which will happen for all recursive calls since the sightline will start on a wall
        trimmedSightline = 
            LineSegment2d.scaleAbout (LineSegment2d.endPoint sightline) 0.999 sightline    
    in
    Polygon2d.edges roomShape
        |> List.map (\e -> 
            (LineSegment2d.intersectionPoint trimmedSightline e
                |> Maybe.map (Tuple.pair e)))
        |> Maybe.orList
        |> Maybe.andThen (\(e, p) -> LineSegment2d.direction e |> Maybe.map (\d -> (e, p, d)))
        |> Maybe.map (\(wall, point, dir) -> 
            { wall = wall, point = point, axis = Axis2d.withDirection dir point })


reflectedRooms : LineSegment -> Polygon -> List Polygon -> List Polygon
reflectedRooms sightline room roomsAcc = 
    findIntersection sightline room
        |> Maybe.map (\inter -> 
            reflectedRooms 
                (LineSegment2d.from inter.point (LineSegment2d.endPoint sightline))
                (room |> Polygon2d.mirrorAcross inter.axis)
                (room :: roomsAcc))
        |> Maybe.withDefault (room :: roomsAcc)

type alias Intersection = 
    { axis : Axis
    , wall : LineSegment
    , point : Point
    }

angleDiff : Point2d u c -> Point2d u c -> Point2d u c -> Maybe Angle
angleDiff pivot p1 p2 = 
    let
        getAngle p = 
            Direction2d.from pivot p 
                |> Maybe.map Direction2d.toAngle
    in
        Maybe.map2 Quantity.difference (getAngle p1) (getAngle p2)



 -- SKETCHING NEW TYPES -- 


type ApparentRoom 
    = ActualRoom Room
    | ReflectionOf ApparentRoom Intersection

type alias Room = 
    { roomShape : Polygon }

actual model = ActualRoom { roomShape = model.roomShape }

firstReflection intersection1 model =
    ReflectionOf (actual model) intersection1

secondReflection intersection1 intersection2 model = 
    ReflectionOf (firstReflection intersection1 model) intersection2

apparentRoomShape : ApparentRoom -> Polygon
apparentRoomShape ar = 
    case ar of 
        ActualRoom room -> room.roomShape
        ReflectionOf twinRoom inter -> 
            apparentRoomShape twinRoom
                |> Polygon2d.mirrorAcross inter.axis

{-
sightlinePathSegments : ApparentRoom -> List LineSegment
sightlinePathSegments ar = 
    case ar of 
        ActualRoom room -> 
            -- TODO the line from viewerPos to the first intersection
        ReflectionOf _ inter ->
            -- TODO the previous lines plus the line from inter to the next intersection
-}

type alias Roome = 
    { wallShape : Polygon2d Meters SceneCoords 
    , sightStart : Point2d Meters SceneCoords
    , sightEnd : SightEnd
    }

type SightEnd
    = TooFar (Point2d Meters SceneCoords)
    | Bounce Intersection Roome


-- type alias Foo =
--     { sightStart : Point2d Meters SceneCoords 
--     , roomShape : Polygon2d Meters SceneCoords
--     , bounces : List Bounce 
--     }

-- type alias Bounce =
--     { intersection : Intersection
--     , reflection : 
--     }