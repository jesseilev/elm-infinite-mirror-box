module Sightray exposing (..)

import Angle exposing (Angle)
import Array exposing (Array)
import Axis2d
import Circle2d
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
import Room
import RoomItem exposing (RoomItem)
import Shared exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Quantity exposing (Quantity)
import Html.Attributes exposing (start)
import Point2d exposing (distanceFrom)
import Angle

type alias Sightray = 
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

noBounces : Point -> RayEnd -> Sightray
noBounces startPos end = 
    Sightray startPos [] end

fromRoomAndProjectedPath : Room.Model -> LineSegment -> Sightray
fromRoomAndProjectedPath room projectedPath = 
    let 
        (projectedStart, projectedEnd) =
            LineSegment2d.endpoints projectedPath

        nextProjectedPath bounce =
            LineSegment2d.from bounce.point projectedEnd
                |> LineSegment2d.mirrorAcross bounce.axis

        recurse bounce = 
            fromRoomAndProjectedPath room (nextProjectedPath bounce)
                |> (\ray -> { ray | startPos = projectedStart } )
                |> addBounce bounce

    in
    case nextIntersection room projectedPath of
        Just (IntersectMirror bounce) -> 
            recurse bounce
        Just (IntersectItem point item) -> 
            noBounces projectedStart (EndAtItem point item)
        _ ->
            noBounces projectedStart (TooFar projectedEnd)
                -- TODO dont hardcode the start and end types
 

 -- Transformations --

addBounce : MirrorBounce -> Sightray -> Sightray
addBounce bounce rp =
    { rp | bounces = bounce :: rp.bounces }

mirrorAcross : Axis -> Sightray -> Sightray
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
    , end = updateEndPos reflectPoint ray.end 
    }


updateEndPos : (Point -> Point) -> RayEnd -> RayEnd
updateEndPos upd end =
    let newPos = upd (getEndPos end) in
    case end of 
        TooFar _ -> TooFar newPos
        EndAtItem _ item -> EndAtItem newPos item

-- Properties --

endPos : Sightray -> Point 
endPos = 
    .end >> getEndPos

getEndPos : RayEnd -> Point 
getEndPos end =
    case end of 
        TooFar p -> p
        EndAtItem p _ -> p

endItem : Sightray -> Maybe RoomItem
endItem ray = 
    case ray.end of 
        TooFar _ -> Nothing 
        EndAtItem _ item -> Just item

nextIntersection : Room.Model -> LineSegment -> Maybe Intersection
nextIntersection room projectedSightLine =
    let
        -- "trim" off the very beginning of the sightline by shrinking it slightly
        -- we dont want to count the start point as an intersection
        -- which will happen for all recursive calls since the sightline will start on a wall
        trimmedSightline = 
            LineSegment2d.scaleAbout (LineSegment2d.endPoint projectedSightLine) 0.999 projectedSightLine   

        startPoint = 
            LineSegment2d.startPoint projectedSightLine

        nextMirrorBounceM = 
            Polygon2d.edges room.wallShape
                |> List.map (\e -> 
                    (LineSegment2d.intersectionPoint trimmedSightline e
                        |> Maybe.map (Tuple.pair e)))
                |> Maybe.orList
                |> Maybe.andThen (\(e, p) -> LineSegment2d.direction e |> Maybe.map (\d -> (e, p, d)))
                |> Maybe.map (\(wall, point, dir) -> 
                    { wall = wall, point = point, axis = Axis2d.withDirection dir point })
        
        checkItemIntersection item point = 
            if RoomItem.containsPoint point item then 
                Just (item, point, Point2d.distanceFrom point startPoint) 
            else 
                Nothing

        isInitialPlayerItem (item, point, distance) = 
            startPoint == room.viewerPos
                && item == Room.playerItem room
                && (distance |> Quantity.lessThan RoomItem.radius)

        itemHitM =
            nextMirrorBounceM 
                |> Maybe.map (.point >> LineSegment2d.from startPoint)
                |> Maybe.withDefault projectedSightLine
                |> Shared.segmentSamplePoints
                |> List.lift2 checkItemIntersection (Room.allItems room) 
                |> Maybe.values
                |> List.sortBy (\(_, _, distance) -> Quantity.unwrap distance)
                |> List.filter (\info -> not (isInitialPlayerItem info))
                -- |> Shared.debugLogF List.length "hit items length"
                |> List.head

    in
        case itemHitM of 
            Just (item, point, _) -> Just (IntersectItem point item)
            Nothing -> Maybe.map IntersectMirror nextMirrorBounceM

type Intersection 
    = IntersectMirror MirrorBounce
    | IntersectItem Point RoomItem

-- tail : Sightray -> Maybe (MirrorBounce, Sightray)
-- tail ray =
--     case ray.bounces of 
--         [] -> Nothing 
--         b :: bs ->
--             { start = MirrorProjection b, bounces = bs , end = ray.end }
--                 |> (\r -> Just (b, r))

-- -- all the steps in the unfolding animation 
-- -- from totally real at the beginning to totally projected at the end
-- unravel : Sightray -> List Sightray
-- unravel ray =
--     ray :: 
--         (tail ray 
--             |> Maybe.map (\(bounce, tailRay) -> 
--                 mirrorAcross bounce.axis tailRay |> unravel
--             )
--             |> Maybe.withDefault []
--         )
-- -- TODO refactor using
-- -- tails ray == [ ray, tail ray, tail tail ray, ... ]
-- -- unravel ray = tails ray |> fold (mirror each thing across the previous bounce)

uncurl : Sightray -> Maybe Sightray 
uncurl ray = 
    projectionsAndReflections ray
        |> (\(projs, refs) -> 
            List.head refs 
                |> Maybe.map .axis
                |> Maybe.map (\axis -> 
                    ( List.map (bounceMirrorAcross axis) refs
                    , updateEndPos (Point2d.mirrorAcross axis) ray.end
                    )
                )
                |> Maybe.map (\(newRefs, newEnd) -> 
                    { ray | bounces = projs ++ newRefs, end = newEnd }
                )
        )

uncurledSeries : Sightray -> List Sightray -- TODO nonempty list?
uncurledSeries = 
    List.iterate uncurl

hallway : Room.Model -> Sightray -> List Room.Model
hallway room ray = 
    let
        reflectRoom : Room.Model -> List MirrorBounce -> Room.Model
        reflectRoom r projs = 
            List.foldl (\proj accRoom -> accRoom |> Room.mirrorAcross proj.axis) r projs
    in
    projections ray 
        |> List.inits 
        |> List.map (reflectRoom room)

projectionsAndReflections : Sightray -> (List MirrorBounce, List MirrorBounce)
projectionsAndReflections ray = 
    let dropNeighborInfo = List.map (\(_, bounce, _) -> bounce) in
    bouncesWithNeighborPoints ray
        |> List.span (\(prev, bounce, next) -> 
            Maybe.map2 (Direction2d.equalWithin (Angle.degrees 0.001))
                (Direction2d.from prev bounce.point)
                (Direction2d.from bounce.point next)
                |> Maybe.withDefault False
        )
        |> Tuple.mapBoth dropNeighborInfo dropNeighborInfo

projections = 
    projectionsAndReflections >> Tuple.first

reflections = 
    projectionsAndReflections >> Tuple.second

vertices : Sightray -> List Point 
vertices ray = -- TODO nonempty list?
    ray.startPos :: (List.map .point ray.bounces) ++ [ getEndPos ray.end ]

polyline : Sightray -> Polyline
polyline ray = 
    ray |> vertices |> Polyline2d.fromVertices

length : Sightray -> Length
length ray =
    ray |> polyline |> Polyline2d.length

bouncesWithNeighborPoints : Sightray -> List (Point, MirrorBounce, Point)
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
                        |> Maybe.withDefault (getEndPos ray.end)
                    )
                )
    in
    ray.bounces 
        |> Array.fromList 
        |> mapTheArray
        |> Array.toList

bouncesWithAngles : Sightray -> List (MirrorBounce, Angle)
bouncesWithAngles ray = 
    ray 
        |> bouncesWithNeighborPoints
        |> List.map (\(prev, bounce, next) -> 
            Shared.angleDiff bounce.point prev next 
                |> Maybe.withDefault (Angle.degrees 0 |> Debug.log "angle fucked up") -- TODO deal with this better?
                |> (\a -> Angle.degrees 180 |> Quantity.minus a |> Quantity.multiplyBy 0.5)
                |> (\a -> (bounce, a))
        )



interpolateFrom : Sightray -> Sightray -> Float -> Sightray
interpolateFrom ray1 ray2 pct =
    { startPos = Shared.interpolatePointFrom ray1.startPos ray2.startPos pct
    , bounces = Shared.interpolateLists interpolateBounceFrom ray1.bounces ray2.bounces pct
    , end = ray1.end |> updateEndPos (\ep1 ->
        Shared.interpolatePointFrom ep1 (getEndPos ray2.end) pct)
    }

interpolateBounceFrom : MirrorBounce -> MirrorBounce -> Float -> MirrorBounce
interpolateBounceFrom bounce1 bounce2 pct = 
    { wall = Shared.interpolateLineFrom bounce1.wall bounce2.wall pct
    , axis = Shared.interpolateAxisFrom bounce1.axis bounce2.axis pct
    , point = Shared.interpolatePointFrom bounce1.point bounce2.point pct 
    }

interpReflect : InterpolatedReflection Sightray
interpReflect axis pct ray = 
    { ray 
        | startPos = interpReflectPoint axis pct ray.startPos
        , bounces = List.map (interpReflectBounce axis pct) ray.bounces
        , end = updateEndPos (interpReflectPoint axis pct) ray.end
    }

interpReflectBounce : InterpolatedReflection MirrorBounce
interpReflectBounce axis pct bounce =
    let 
        newWall = interpReflectLine axis pct bounce.wall 
        newAxis = Axis2d.throughPoints (LineSegment2d.startPoint newWall) 
            (LineSegment2d.endPoint newWall)
            |> Maybe.withDefault bounce.axis
    in
    { wall = newWall 
    , axis = newAxis
    , point = interpReflectPoint axis pct bounce.point 
    }

bounceMirrorAcross : Axis -> MirrorBounce -> MirrorBounce
bounceMirrorAcross axis =
    interpReflectBounce axis 1

-- VIEW 

view : Sightray -> Svg msg
view = 
    viewWithAttrs lineAttrsDefault

viewWithAttrs attrs ray = 
    ray 
        |> polyline 
        |> (\pl -> 
            Svg.g [] 
                [ 
                    -- Svg.polyline2d [ Attr.stroke "white", Attr.strokeWidth "0.1", Attr.fill "none" ] pl
                -- , 
                Svg.polyline2d attrs pl
                ]
        )

lineAttrsDefault = lineAttrs "black" "0.01"

lineAttrs : String -> String -> List (Svg.Attribute msg)
lineAttrs color width = 
    [ Attr.fill "none" 
    , Attr.stroke color
    , Attr.strokeWidth width
    , Attr.strokeDasharray "0.05"
    , Attr.strokeDashoffset "0.0"
    ]

viewSamplePoints : Sightray -> Svg msg 
viewSamplePoints ray = 
    ray 
        |> polyline
        |> Polyline2d.segments
        |> List.map Shared.segmentSamplePoints
        |> List.concat
        |> List.map (\p -> Svg.circle2d [ Attr.fill "red" ] 
            (Circle2d.atPoint p (Length.meters 0.02)))
        |> Svg.g []