module SightRay exposing 
    ( AngleLabels(..)
    , beam
    , endItem
    , fromRoomAndProjectedPath
    , hallway
    , interpolateFrom
    , length
    , RayEnd(..)
    , viewDistanceLabel
    , viewProjectionWithOptions
    , viewWithOptions
    , SightRay
    , uncurl
    , uncurledSeries
    )

import Angle exposing (Angle)
import Arc2d
import Array exposing (Array)
import Axis2d
import Circle2d
import Color exposing (Color)
import Color.Convert as Color
import Color.Interpolate as Color
import Direction2d
import Geometry.Svg as Svg
import Length exposing (Length)
import LineSegment2d exposing (LineSegment2d)
import List.Extra as List
import List.Nonempty
import Maybe.Extra as Maybe
import Point2d
import Polygon2d
import Polyline2d
import Quantity exposing (Quantity)
import Room exposing (Room)
import RoomItem exposing (RoomItem)
import Shared exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Triangle2d
import Vector2d 


 -- Types -- 

{-| 
`SightRay` represents a trace of the viewer's gaze looking out into the room.

Breaks down into 4 stages:
1. It starts at the viewer's location, 
2. passes through 0 or more mirrors (if we are showing the success animation),
3. reflects off of 0 or more mirrors,
4. and finally ends when it either: 
    - reaches the desired distance 
    - or terminates early due to hitting an item in the room.

A few things to note:
- The distinction between steps 2 and 3 above is not enforced at the type level.
- The "actual ray of light" travels in the opposite direction, 
starting at the SightRay's end point and arriving at the viewer's location.
-}
type alias SightRay = 
    { startPos : Point
    , bounces : List MirrorBounce 
    , end : RayEnd 
    }

type RayEnd 
    = TooFar Point
    | EndAtItem Point RoomItem

type alias MirrorBounce = 
    { axis : Axis
    , wall : LineSegment
    , point : Point
    }


-- Constructors -- 

noBounces : Point -> RayEnd -> SightRay
noBounces startPos end = 
    SightRay startPos [] end


{-| 
Construct a SightRay from the Room where the viewer is standing
and a "projected sightline", ie a straight line segment describing where the 
viewer's gaze *would* travel if it were to continue straight and never hit anything.
-}
fromRoomAndProjectedPath : Room -> LineSegment -> SightRay
fromRoomAndProjectedPath room projectedSightline = 
    let 
        (projectedStart, projectedEnd) =
            LineSegment2d.endpoints projectedSightline

        nextProjectedPath bounce =
            LineSegment2d.from bounce.point projectedEnd
                |> LineSegment2d.mirrorAcross bounce.axis

        recurse bounce = 
            fromRoomAndProjectedPath room (nextProjectedPath bounce)
                |> (\ray -> { ray | startPos = projectedStart } )
                |> addBounce bounce

    in
    case nextIntersection room projectedSightline of
        Just (IntersectMirror bounce) -> 
            recurse bounce
        Just (IntersectItem point item) -> 
            noBounces projectedStart (EndAtItem point item)
        _ ->
            noBounces projectedStart (TooFar projectedEnd)

{-| 
helper function used by `fromRoomAndProjectedPath`
Given the room and projected sightline, find the next wall or item, if any,
that the sightline collides with
-}
nextIntersection : Room -> LineSegment -> Maybe Intersection
nextIntersection room projectedSightline =
    let
        -- "trim" off the very beginning of the sightline by shrinking it slightly
        -- we dont want to count the start point as an intersection
        -- which will happen for all recursive calls since the sightline will start on a wall
        trimmedSightline = 
            LineSegment2d.scaleAbout (LineSegment2d.endPoint projectedSightline) 0.999 projectedSightline   

        nextMirrorBounceM = 
            Polygon2d.edges room.wallShape
                |> List.map (\e -> 
                    (LineSegment2d.intersectionPoint trimmedSightline e
                        |> Maybe.map (Tuple.pair e)))
                |> Maybe.orList
                |> Maybe.andThen (\(e, p) -> LineSegment2d.direction e |> Maybe.map (\d -> (e, p, d)))
                |> Maybe.map (\(wall, point, dir) -> 
                    { wall = wall, point = point, axis = Axis2d.withDirection dir point })

        startPoint = 
            LineSegment2d.startPoint projectedSightline
        
        checkItemIntersection item point = 
            if RoomItem.containsPoint point item then 
                Just (item, point, Point2d.distanceFrom point startPoint) 
            else 
                Nothing

        isInitialPlayerItem (item, point, distance) = 
            startPoint == room.playerItem.pos
                && (distance |> Quantity.lessThan RoomItem.radius)

        itemHitM =
            nextMirrorBounceM 
                |> Maybe.map (.point >> LineSegment2d.from startPoint)
                |> Maybe.withDefault projectedSightline
                |> Shared.segmentSamplePoints
                |> List.lift2 checkItemIntersection (Room.allItems room) 
                |> Maybe.values
                |> List.sortBy (\(_, _, distance) -> Quantity.unwrap distance)
                |> List.filter (\info -> not (isInitialPlayerItem info))
                |> List.head

    in
        case itemHitM of 
            Just (item, point, _) -> Just (IntersectItem point item)
            Nothing -> Maybe.map IntersectMirror nextMirrorBounceM

type Intersection 
    = IntersectMirror MirrorBounce
    | IntersectItem Point RoomItem

 

 -- Transformations --

addBounce : MirrorBounce -> SightRay -> SightRay
addBounce bounce rp =
    { rp | bounces = bounce :: rp.bounces }

mirrorAcross : Axis -> SightRay -> SightRay
mirrorAcross axis ray =
    let 
        reflectPoint : Point -> Point
        reflectPoint = Point2d.mirrorAcross axis

        reflectBounce : MirrorBounce -> MirrorBounce
        reflectBounce bounce = 
            { point = reflectPoint bounce.point
            , wall = LineSegment2d.mirrorAcross axis bounce.wall
            , axis = Axis2d.mirrorAcross axis bounce.axis 
            }
    in
    { startPos = reflectPoint ray.startPos
    , bounces = List.map reflectBounce ray.bounces
    , end = mirrorEndAcross axis ray.end
    }

mirrorEndAcross : Axis -> RayEnd -> RayEnd 
mirrorEndAcross axis end = 
    let reflectPoint = Point2d.mirrorAcross axis in
    case end of 
        TooFar pos -> TooFar (reflectPoint pos)
        EndAtItem pos item -> 
            EndAtItem (reflectPoint pos) 
                (item |> RoomItem.setPos (reflectPoint pos))


updateEndPos : (Point -> Point) -> RayEnd -> RayEnd
updateEndPos upd end =
    let newPos = upd (endPosFromEnd end) in
    case end of 
        TooFar _ -> TooFar newPos
        EndAtItem _ item -> EndAtItem newPos item


-- Properties --

endPos : SightRay -> Point 
endPos = 
    .end >> endPosFromEnd

endPosFromEnd : RayEnd -> Point 
endPosFromEnd end =
    case end of 
        TooFar p -> p
        EndAtItem p _ -> p

endItem : SightRay -> Maybe RoomItem
endItem ray = 
    case ray.end of 
        TooFar _ -> Nothing 
        EndAtItem _ item -> Just item

{-|

-}
uncurl : SightRay -> Maybe SightRay 
uncurl ray = 
    projectionsAndReflections ray
        |> (\(projs, refs) -> 
            List.head refs 
                |> Maybe.map .axis
                |> Maybe.map (\axis -> 
                    ( List.map (bounceMirrorAcross axis) refs
                    , mirrorEndAcross axis ray.end
                    )
                )
                |> Maybe.map (\(newRefs, newEnd) -> 
                    { ray | bounces = projs ++ newRefs, end = newEnd }
                )
        )

uncurledSeries : SightRay -> List SightRay 
uncurledSeries = 
    List.iterate uncurl

hallway : Room -> SightRay -> List Room
hallway room ray = 
    let
        reflectRoom : Room -> List MirrorBounce -> Room
        reflectRoom r projs = 
            List.foldl (\proj accRoom -> accRoom |> Room.mirrorAcross proj.axis) r projs
    in
    projections ray 
        |> List.inits 
        |> List.map (reflectRoom room)

projectionsAndReflections : SightRay -> (List MirrorBounce, List MirrorBounce)
projectionsAndReflections ray = 
    let dropNeighborInfo = List.map (\(_, bounce, _) -> bounce) in
    bouncesWithNeighborPoints ray
        |> List.span isProjection
        |> Tuple.mapBoth dropNeighborInfo dropNeighborInfo

isProjection (prev, bounce, next) =
    Maybe.map2 (Direction2d.equalWithin (Angle.degrees 0.001))
        (Direction2d.from prev bounce.point)
        (Direction2d.from bounce.point next)
        |> Maybe.withDefault False

projections : SightRay -> List MirrorBounce
projections = 
    projectionsAndReflections >> Tuple.first

reflections : SightRay -> List MirrorBounce
reflections = 
    projectionsAndReflections >> Tuple.second

vertices : SightRay -> List Point 
vertices ray = -- TODO nonempty list?
    ray.startPos :: (List.map .point ray.bounces) ++ [ endPosFromEnd ray.end ]

polyline : SightRay -> Polyline
polyline ray = 
    ray |> vertices |> Polyline2d.fromVertices

length : SightRay -> Length
length ray =
    ray |> polyline |> Polyline2d.length

bouncesWithNeighborPoints : SightRay -> List (Point, MirrorBounce, Point)
bouncesWithNeighborPoints ray = 
    let
        mapTheArray : Array MirrorBounce -> Array (Point, MirrorBounce, Point)
        mapTheArray bounces = 
            bounces 
                |> Array.indexedMap (\i bounce ->
                    (Array.get (i - 1) bounces 
                        |> Maybe.map .point 
                        |> Maybe.withDefault ray.startPos
                    , bounce
                    , Array.get (i + 1) bounces 
                        |> Maybe.map .point
                        |> Maybe.withDefault (endPosFromEnd ray.end)
                    )
                )
    in
    ray.bounces 
        |> Array.fromList 
        |> mapTheArray
        |> Array.toList


{-|
Returns a handful of `SightRay`s that together comprise the yellow beam of light we show on screen.
Takes the "real" ray as input and rotates it a bit in either direction, with an angle such that,
at the desired distance away, the width of the beam will equal the diameter of a `RoomItem`.
-}
beam : Room -> LineSegment -> List SightRay
beam room sightline = 
    let 
        (sightStart, sightEnd) = LineSegment2d.endpoints sightline
        beamRay angle =
            Direction2d.from sightStart sightEnd |> Maybe.map (\sightDirection ->
                Direction2d.rotateBy angle sightDirection
                    |> (\dir -> 
                        Shared.projectedSightline sightStart dir 
                            (LineSegment2d.length sightline)
                    )
                    |> fromRoomAndProjectedPath room
            )
        
        numRays = 10

        angleIncrement =  
            let 
                sAdjacent = LineSegment2d.length sightline |> Quantity.unwrap
                sOpposite = RoomItem.radius |> Quantity.unwrap
            in
            atan (sOpposite / sAdjacent) / (toFloat numRays * 0.5)
    in
    List.initialize numRays (\i -> (toFloat i - (toFloat numRays * 0.5)) * angleIncrement)
        |> List.map Angle.radians
        |> List.map beamRay
        |> Maybe.values

angleArcs : SightRay -> List (Arc, MirrorBounce, Arc)
angleArcs ray = 
    let 
        mkWedge bounce neighbor = 
            let 
                pointOnAxis modDirection = 
                    Point2d.translateIn (Axis2d.direction bounce.axis |> modDirection) 
                        angleArcRadius
                        bounce.point
            in 
            Shared.takeMin (Point2d.distanceFrom neighbor) 
                (pointOnAxis identity) 
                (pointOnAxis Direction2d.reverse) 
                |> (\poa -> 
                    Shared.angleDiff bounce.point poa neighbor
                        |> Maybe.withDefault (Angle.degrees 0)
                        |> Shared.normalizeAngle
                        |> (\angle ->  Arc2d.sweptAround bounce.point angle poa)
                )
                
    in
    bouncesWithNeighborPoints ray 
        |> List.map (\(prev, bounce, next) -> ( mkWedge bounce prev, bounce, mkWedge bounce next )) 



angleArcRadius : Length
angleArcRadius = Length.meters 0.25

interpolateFrom : SightRay -> SightRay -> Float -> SightRay
interpolateFrom ray1 ray2 pct =
    { startPos = Shared.interpolatePointFrom ray1.startPos ray2.startPos pct
    , bounces = Shared.interpolateLists interpolateBounceFrom ray1.bounces ray2.bounces pct
    , end = ray1.end |> updateEndPos (\ep1 ->
        Shared.interpolatePointFrom ep1 (endPosFromEnd ray2.end) pct)
    }

interpolateBounceFrom : MirrorBounce -> MirrorBounce -> Float -> MirrorBounce
interpolateBounceFrom bounce1 bounce2 pct = 
    { wall = Shared.interpolateLineFrom bounce1.wall bounce2.wall pct
    , axis = Shared.interpolateAxisFrom bounce1.axis bounce2.axis pct
    , point = Shared.interpolatePointFrom bounce1.point bounce2.point pct 
    }

bounceMirrorAcross : Axis -> MirrorBounce -> MirrorBounce
bounceMirrorAcross axis bounce =
    let 
        newWall = LineSegment2d.mirrorAcross axis bounce.wall 
        newAxis = Axis2d.throughPoints (LineSegment2d.startPoint newWall) 
            (LineSegment2d.endPoint newWall)
            |> Maybe.withDefault bounce.axis
    in
    { wall = newWall 
    , axis = newAxis
    , point = Point2d.mirrorAcross axis bounce.point 
    }


-- VIEW 

type alias ViewOptions msg = 
    { attributes : List (Svg.Attribute msg)
    , projectionAttributes : List (Svg.Attribute msg)
    , angleLabels : AngleLabels
    , zoomScale : Float
    }

type AngleLabels 
    = NoAngles 
    | AllAngles 

defaultOptions : ViewOptions msg 
defaultOptions = 
    { attributes = [] 
    , projectionAttributes = []
    , angleLabels = AllAngles
    , zoomScale = 1
    }

view : SightRay -> Svg msg
view = 
    viewWithOptions defaultOptions

viewWithOptions : ViewOptions msg -> SightRay -> Svg msg
viewWithOptions options ray =
    let (projs, refs) = projectionsAndReflections ray in
    Svg.g []
        [ projs
            |> List.map .point 
            |> (\vs -> 
                ray.startPos :: vs 
                    ++ (refs 
                        |> List.head 
                        |> Maybe.map .point 
                        |> Maybe.withDefault (endPos ray)
                        |> (\p -> [p])
                    )
            )
            |> Polyline2d.fromVertices
            |> Svg.polyline2d (lineAttrs options.zoomScale options.attributes)
        , refs 
            |> List.map .point 
            |> (\vs -> vs ++ [endPos ray])
            |> Polyline2d.fromVertices
            |> Svg.polyline2d (lineAttrsDefault options.zoomScale ++ options.attributes)
        , case options.angleLabels of 
            AllAngles -> 
                Svg.g [] 
                    [ viewAngles ray
                    -- , distanceLabel ray
                    ]
            _ -> Shared.svgEmpty
        ]

viewDistanceLabel : SightRay -> Svg msg
viewDistanceLabel ray = 
    let 
        endxy = 
            case ray.end of 
                TooFar pos -> pos 
                EndAtItem _ item -> item.pos
    in
    Svg.g [] 
        [ Svg.circle2d
            [ Attr.fill "none", Attr.strokeWidth "0.01", Attr.stroke Shared.colors.greyMedium ]
            (Circle2d.atPoint endxy RoomItem.radius)
        , Shared.viewLabel Shared.colors.greyMedium endxy (Vector2d.meters 0.1 0.3) 
            (length ray 
                |> Length.inCentimeters 
                |> (\c -> c / 100) 
                |> round
                |> String.fromInt
                |> (\s -> s ++ " m")
            )
        ]

lineAttrs zoomScale customAttrs =
    lineAttrsDefault zoomScale ++ customAttrs

lineAttrsDefault : Float -> List (Svg.Attribute msg)
lineAttrsDefault zoomScale = 
    [ Attr.fill "none" 
    , Attr.stroke "black"
    , Attr.strokeWidth <| Shared.floatAttributeForZoom zoomScale 0.01
    , Attr.strokeDasharray <| Shared.floatAttributeForZoom zoomScale 0.1
    , Attr.strokeDashoffset <| Shared.floatAttributeForZoom zoomScale 0.0
    ]

viewProjectionWithOptions : ViewOptions msg -> SightRay -> Svg msg 
viewProjectionWithOptions options ray = 
    ray 
        |> reflections -- final projection point is the first reflection point
        |> List.head 
        |> Maybe.map .point
        |> Maybe.withDefault (endPos ray)
        |> (LineSegment2d.from ray.startPos)
        |> Svg.lineSegment2d (lineAttrs options.zoomScale options.attributes)

viewAngles : SightRay -> Svg msg
viewAngles ray = 
    angleArcs ray
        |> List.map (\(arc1, bounce, arc2) -> 
            [ viewAngle bounce arc1, viewAngle bounce arc2 ]
        )
        |> List.concat
        |> Svg.g []


viewAngle : MirrorBounce -> Arc -> Svg msg
viewAngle bounce arc =
    let 
        angle = Arc2d.sweptAngle arc 

        color = angleColor angle |> Color.colorToHex

        arcCenter = Arc2d.centerPoint arc

        sideLine p = 
            Svg.lineSegment2d 
                [ Attr.fill "none"
                , Attr.stroke Shared.colors.greyDark
                , Attr.strokeWidth "0.02" 
                , Attr.strokeDasharray "0.02"
                ]
                (LineSegment2d.from arcCenter p
                    |> LineSegment2d.scaleAbout arcCenter 1.5
                )
    in
    Svg.g []
        [ Svg.arc2d 
            [ Attr.fill color
            , Attr.stroke Shared.colors.greyDark
            , Attr.strokeWidth "0.01"
            ] 
            arc
        , Svg.triangle2d [ Attr.fill color ] (Shared.triangleForArc arc)
        , Shared.viewLabel Shared.colors.greyDark
            (LineSegment2d.from arcCenter (Arc2d.midpoint arc)
                |> (\l -> LineSegment2d.interpolate l 0.7)
            )
            (Vector2d.rTheta (Length.meters 0.25) 
                (Direction2d.perpendicularTo (Axis2d.direction (Axis2d.reverse bounce.axis))
                    |> Direction2d.toAngle
                )
            )
            (angle
                |> Quantity.abs 
                |> Angle.inDegrees 
                |> round 
                |> String.fromInt 
                |> (\s -> s ++ "º")
            )
        , sideLine (Arc2d.startPoint arc)
        , sideLine (Arc2d.endPoint arc)
        ]
        
angleColor : Angle -> Color
angleColor angle = 
    Color.interpolate Color.RGB
        (Shared.colors.green1 |> Color.hexToColor |> Result.withDefault Color.green)
        (Shared.colors.yellow1 |> Color.hexToColor |> Result.withDefault Color.blue)
        (angle 
            |> Quantity.abs 
            |> Quantity.unwrap
            |> (\a -> a / (Quantity.unwrap (Angle.degrees 90)))
        )
