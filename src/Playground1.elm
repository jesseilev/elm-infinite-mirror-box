module Playground1 exposing (..)

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import Browser
import Circle2d
import Convert
import Direction2d exposing (Direction2d)
import Element as El exposing (Element)
import Element.Border as Border
import Element.Events
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
import Axis2d exposing (relativeTo)
import Pixels exposing (pixel)
import String exposing (lines)
import String exposing (trim)
import Element.Font exposing (tabularNumbers)

main = 
    Browser.element
        { init = init
        , update = update   
        , subscriptions = subscriptions
        , view = view
        }

{- origin point at the top left of the frame, x -> right, y -> down -}
type TopLeftCoords = TopLeftCoords 

{- origin point in the center of the frame, x -> right, y -> up -}
type SceneCoords = SceneCoords

type alias Model = 
    { roomShape : Polygon2d Meters SceneCoords
    , targetDistance : Quantity Float Meters
    , viewerPos : Point2d Meters SceneCoords
    , viewerAngle : Angle
    , mouseDragPos : Maybe (Point2d Meters SceneCoords)
    , mouseDown : Bool
    , clickPosDebug : Point2d Meters SceneCoords
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
    , targetDistance = Length.meters 15.0
    , viewerPos = Point2d.meters 1.0 -0.5
    , viewerAngle = Angle.degrees 50
    , mouseDragPos = Nothing
    , mouseDown = False
    , clickPosDebug = Point2d.origin
    , zoomScale = 1
    }
        |> noCmds

        
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


angleDiff : Point2d u c -> Point2d u c -> Point2d u c -> Maybe Angle
angleDiff pivot p1 p2 = 
    let
        getAngle p = 
            Direction2d.from pivot p 
                |> Maybe.map Direction2d.toAngle
    in
        Maybe.map2 Quantity.difference (getAngle p1) (getAngle p2)

noCmds x = ( x, Cmd.none )


subscriptions _ = 
    Sub.none
        
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
                MouseDragAt (mouseToSceneCoords event.pointer.offsetPos)
            else 
                NoOp)
        , Wheel.onWheel (\event -> AdjustZoom event.deltaY)
        ]
        [ Svg.svg 
            [ Attr.width (constants.containerWidth |> String.fromFloat)
            , Attr.height (constants.containerHeight |> String.fromFloat)
            ]
            [ viewScene model ]
            -- [ diagram model |> svgToSceneCoords sceneFrame
            -- ]
        ]

viewFrame clr = 
    Svg.g [] 
        [ Svg.rectangle2d 
            [ Attr.stroke clr
            , Attr.strokeWidth "0.02" -- in meters. TODO use typedsvg
            , Attr.fill "none" 
            ]
            (Rectangle2d.from Point2d.origin (Point2d.meters 0.5 1.0))
        , Svg.circle2d [ Attr.fill "black" ]
            (Circle2d.atOrigin (Length.meters 0.05))
        ]

projectedSightline : Model -> LineSegment2d Meters SceneCoords
projectedSightline model =
    LineSegment2d.fromPointAndVector model.viewerPos
        (Vector2d.withLength model.targetDistance 
            (Frame2d.yDirection (viewerFrame model)))
        
viewScene : Model -> Svg Msg 
viewScene model = 
    let 
        tree pos = 
            Svg.circle2d
            [ Attr.fill "green" 
            , Mouse.onClick (\event -> MouseClickAt (mouseToSceneCoords event.offsetPos))
            ]
            (Circle2d.atPoint pos (Length.meters 0.1))
    in
    Svg.g [] 
        [ viewFrame "grey"
            -- |> Svg.relativeTo topLeftFrame
        , viewFrame "purple"
            |> Svg.placeIn (roomFrame model)
        , viewFrame "orange" 
            |> Svg.placeIn (viewerFrame model)
        , Svg.polygon2d 
                [ Attr.strokeWidth "0.02" 
                , Attr.fill "none"
                , Attr.stroke "grey"
                ]
                (model.roomShape |> Polygon2d.placeIn (roomFrame model))
        , Svg.circle2d
            [ Attr.fill "red" ]
            (Circle2d.atPoint model.clickPosDebug (Length.meters 0.05))
        , tree (Point2d.meters -0.5 0.3)
        , tree (Point2d.meters 0.2 0.9)
        , tree (Point2d.meters 1.1 -0.4)
        , Svg.lineSegment2d
            [ Attr.stroke "cyan" 
            , Attr.strokeWidth "0.02"
            ]
            (projectedSightline model)
        , viewReflectedRooms model
        ]
        |> Svg.at pixelsPerMeter
        |> Svg.scaleAbout Point2d.origin model.zoomScale
        |> Svg.relativeTo topLeftFrame


reflectedRooms : LineSegment2d u c -> Polygon2d u c -> List (Polygon2d u c) -> List (Polygon2d u c)
reflectedRooms sightline room roomsAcc = 
    findIntersection sightline room
        |> Maybe.map (\inter -> 
            reflectedRooms 
                (LineSegment2d.from inter.point (LineSegment2d.endPoint sightline))
                (room |> Polygon2d.mirrorAcross inter.axis)
                (room :: roomsAcc))
        |> Maybe.withDefault (room :: roomsAcc)

type alias Intersection u c = 
    { axis : Axis2d u c 
    , wall : LineSegment2d u c 
    , point : Point2d u c 
    }

findIntersection : LineSegment2d u c -> Polygon2d u c -> Maybe (Intersection u c)
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

        reflectionSvg = 
            wallM
                |> Maybe.map reflectedRoom
                |> Maybe.withDefault (Polygon2d.singleLoop [])
                |> Svg.polygon2d [ Attr.fill "yellow" ]
    in
        reflectedRooms (projectedSightline model) model.roomShape []
            |> List.map (Svg.polygon2d 
                [ Attr.fill "none"
                , Attr.stroke "orange"
                , Attr.strokeWidth "0.02" 
                ])
            |> Svg.g [] 
    




-- Frame, Units, Conversions --

pixelsPerMeter = 
    pixels 100 |> Quantity.per (Length.meters 1)

topLeftFrame : Frame2d Pixels SceneCoords { defines : TopLeftCoords }
topLeftFrame = 
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

-- TODO delete this
sceneFrame : Frame2d Pixels TopLeftCoords { defines : SceneCoords }
sceneFrame = 
    Frame2d.atPoint (Point2d.pixels 
        (constants.containerWidth / 2.0) 
        (constants.containerHeight / 2.0))

mouseToSceneCoords : (Float, Float) -> Point2d Meters SceneCoords
mouseToSceneCoords (x, y) = 
    Point2d.pixels x y
        |> Point2d.placeIn topLeftFrame
        |> Point2d.at_ pixelsPerMeter

svgToSceneCoords : Frame2d Pixels globalC { defines : localC } -> Svg msg -> Svg msg
svgToSceneCoords localFrame svg =
    svg 
        |> Svg.mirrorAcross Axis2d.x 
        |> Svg.placeIn localFrame

diagram : Model -> Svg Msg
diagram model =
    let 
        catFrame = 
            Frame2d.atPoint model.viewerPos
                |> Frame2d.rotateBy model.viewerAngle

        catLocation = 
            model.viewerPos

        sightLine = 
            LineSegment2d.fromPointAndVector catLocation
                (Vector2d.withLength model.targetDistance (Frame2d.yDirection catFrame))

        bouncePoint = 
            model.roomShape
                |> Polygon2d.edges
                |> List.map (LineSegment2d.intersectionPoint sightLine)
                |> Maybe.orList
    in
    Svg.g 
        []
        [ Svg.lineSegment2d 
            [ Attr.stroke "blue" 
            , Attr.width "2"
            ]
            (sightLine |> Convert.lineSegmentToPixels)
        , Svg.polygon2d 
            [ Attr.stroke "orange"
            , Attr.strokeWidth "4"
            , Attr.fill "none"
            ]
            (model.roomShape |> Convert.polygonToPixels)
        , bouncePoint
            |> Maybe.map (\p -> 
                Svg.circle2d [ Attr.fill "cyan" ]
                    (Circle2d.atPoint (p |> Convert.pointToPixels) (Pixels.float 5))
            )
            |> Maybe.withDefault svgEmtpy
        , Svg.circle2d [ Attr.fill "purple" ]
            (Circle2d.atPoint (catLocation |> Convert.pointToPixels) (Pixels.float 4))
        -- , Svg.polyline2d
        --     [ Attr.stroke "cyan"
        --     , Attr.strokeWidth "6px"
        --     , Attr.fill "none"
        --     ]
        --     (bouncePath model.roomShape model.targetDistance model.catFrame
        --         |> Convert.polylineToPixels 
        --     )
        ]

svgEmtpy = Svg.g [] []


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

bouncePath : Polygon2d u c -> Quantity Float u -> Frame2d u c d -> Polyline2d u c
bouncePath wallShape gazeDistance frame = 
    let 
        position = 
            Frame2d.originPoint frame

        gazeVector = 
            Vector2d.withLength gazeDistance (Frame2d.yDirection frame)

        endPoint =
            Point2d.translateBy gazeVector position

        gazePathStraight = 
            LineSegment2d.from position endPoint

        bounceM : Maybe { point : Point2d u c, wall: LineSegment2d u c }
        bounceM = 
            wallShape
                |> Polygon2d.edges
                |> List.map (\e -> 
                    LineSegment2d.intersectionPoint gazePathStraight e 
                        |> Maybe.map (\p -> { point = p, wall = e }))
                |> Maybe.orList

        -- gazePathTail =
        --     bounceM
        --         |> Maybe.andThen (\b -> LineSegment2d.direction b.wall |> Maybe.map (\dir -> (b, dir)))
        --         |> Maybe.map (\(b, wallDirection) -> 
        --             LineSegment2d.from b.point endPoint
        --                 |> LineSegment2d.mirrorAcross (Axis2d.through b.point wallDirection))
        
        recurse bounce = 
            let 
                newDistance = 
                    gazeDistance |> Quantity.minus (Point2d.distanceFrom position bounce.point)

                wallDirection = 
                    LineSegment2d.direction bounce.wall 
                        |> Maybe.withDefault Direction2d.y -- todo this is bad

                newDirection = 
                    Frame2d.yDirection frame
                        |> Direction2d.mirrorAcross (Axis2d.through bounce.point wallDirection)

                newFrame = 
                    Frame2d.withYDirection newDirection bounce.point
            in 
                bouncePath wallShape newDistance newFrame

        vertices = 
            bounceM 
                |> Maybe.map (\b -> b.point :: (Polyline2d.vertices (recurse b)))
                |> Maybe.withDefault [ endPoint ]
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