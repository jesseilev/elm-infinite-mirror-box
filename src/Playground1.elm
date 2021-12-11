module Playground1 exposing (..)

import Angle
import Axis2d exposing (Axis2d)
import Browser
import Circle2d
import Convert
import Direction2d exposing (Direction2d)
import Element as El exposing (Element)
import Element.Border as Border
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Length exposing (Length, Meters)
import LineSegment2d exposing (LineSegment2d)
import Maybe.Extra as Maybe
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Vector2d exposing (Vector2d)
import Convert exposing (lengthToPixels)

main = 
    Browser.element
        { init = init
        , update = update   
        , subscriptions = subscriptions
        , view = view
        }

{- origin point at the top left of the frame, x -> right, y -> down -}
type TopLeftCoords = TopLeftCoords 

{- origin point in the center of the frame, x -> right, y -> down -}
type SceneCoords = SceneCoords

type alias Model = 
    { roomShape : Polygon2d Meters SceneCoords
    , catFrame : Frame2d Meters SceneCoords {}
    , targetDistance : Quantity Float Meters
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
    , catFrame = 
        Frame2d.atOrigin
            |> Frame2d.rotateBy (Angle.degrees 315)
    , targetDistance = Length.meters 20.0
    }
        |> noCmds

        
type Msg 
    = NoOp
    | MouseDownAt (Point2d Meters SceneCoords)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        MouseDownAt sceneOffsetPos ->
            { model | catFrame = model.catFrame |> pointXAxisAt sceneOffsetPos }
                |> noCmds

        _ ->
            model |> noCmds


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
    Svg.svg 
        [ Attr.width (constants.containerWidth |> String.fromFloat)
        , Attr.height (constants.containerHeight |> String.fromFloat)
        , Mouse.onDown (\event -> 
            MouseDownAt (Convert.mouseToScene svgContainerFrame event.offsetPos))
        ]
        [ diagram model |> Svg.placeIn svgContainerFrame ]

svgContainerFrame : Frame2d Pixels TopLeftCoords { defines : SceneCoords }
svgContainerFrame = 
    Frame2d.atPoint (Point2d.pixels 
        (constants.containerWidth / 2.0) 
        (constants.containerHeight / 2.0))


diagram : Model -> Svg Msg
diagram model =
    let 
        catLocation = 
            Frame2d.originPoint model.catFrame

        sightLine = 
            LineSegment2d.fromPointAndVector catLocation
                (Vector2d.withLength model.targetDistance (Frame2d.xDirection model.catFrame))

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


pointXAxisAt : Point2d u c -> Frame2d u c {} -> Frame2d u c {}
pointXAxisAt target frame =
    let 
        currentDirection =
            Frame2d.xDirection frame

        targetDirection = 
            Vector2d.from (Frame2d.originPoint frame) target
                |> Vector2d.direction
                |> Maybe.withDefault currentDirection

        angleDiff = 
            Direction2d.angleFrom currentDirection targetDirection
    in 
        Frame2d.rotateBy angleDiff frame

bouncePath : Polygon2d u c -> Quantity Float u -> Frame2d u c d -> Polyline2d u c
bouncePath wallShape gazeDistance frame = 
    let 
        position = 
            Frame2d.originPoint frame

        gazeVector = 
            Vector2d.withLength gazeDistance (Frame2d.xDirection frame)

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
                        |> Maybe.withDefault Direction2d.x

                newDirection = 
                    Frame2d.xDirection frame
                        |> Direction2d.mirrorAcross (Axis2d.through bounce.point wallDirection)

                newFrame = 
                    Frame2d.withXDirection newDirection bounce.point
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