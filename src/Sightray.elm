module Sightray exposing (..)

import Axis2d
import Circle2d
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

type alias Sightray = 
    { start : RayStart 
    , bounces : List MirrorBounce 
    , end : RayEnd 
    }

type RayStart 
    = StartAtPlayer Point
    | MirrorProjection MirrorBounce

type RayEnd 
    = TooFar Point
    | EndAtItem Point RoomItem

type alias MirrorBounce = 
    { axis : Axis
    , wall : LineSegment
    , point : Point
    }



-- Constructors -- 

noBounces : RayStart -> RayEnd -> Sightray
noBounces start end = 
    Sightray start [] end

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
                |> (\ray -> { ray | start = updateStartPos (\_ -> projectedStart) ray.start })
                |> addBounce bounce

    in
    case nextIntersection room projectedPath of
        Just (IntersectMirror bounce) -> 
            recurse bounce
        Just (IntersectItem point item) -> 
            noBounces (StartAtPlayer projectedStart) (EndAtItem point item)
        _ ->
            noBounces (StartAtPlayer projectedStart) (TooFar projectedEnd)
                -- TODO dont hardcode the start and end types
 

 -- Transformations --

addBounce : MirrorBounce -> Sightray -> Sightray
addBounce bounce rp =
    { rp | bounces = bounce :: rp.bounces }

-- prependStart : Point -> Sightray -> Sightray
-- prependStart start rp = 
--     { rp | start = start }
--         -- |> addBounce rp.start

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
    { start = updateStartPos reflectPoint ray.start 
    , bounces = List.map reflectBounce ray.bounces
    , end = updateEndPos reflectPoint ray.end 
    }


updateStartPos : (Point -> Point) -> RayStart -> RayStart
updateStartPos upd start =
    let newPos = upd (startPos start) in
    case start of 
        StartAtPlayer _ -> StartAtPlayer newPos
        MirrorProjection bounce -> MirrorProjection { bounce | point = newPos }

updateEndPos : (Point -> Point) -> RayEnd -> RayEnd
updateEndPos upd end =
    let newPos = upd (endPos end) in
    case end of 
        TooFar _ -> TooFar newPos
        EndAtItem _ item -> EndAtItem newPos item

-- Properties --

startPos : RayStart -> Point
startPos rs = 
    case rs of 
        StartAtPlayer p -> p
        MirrorProjection mb -> mb.point

endPos : RayEnd -> Point 
endPos re =
    case re of 
        TooFar p -> p
        EndAtItem p _ -> p

nextIntersection : Room.Model -> LineSegment -> Maybe Intersection
nextIntersection room projectedPath =
    let
        -- "trim" off the very beginning of the sightline by shrinking it slightly
        -- we dont want to count the start point as an intersection
        -- which will happen for all recursive calls since the sightline will start on a wall
        trimmedSightline = 
            LineSegment2d.scaleAbout (LineSegment2d.endPoint projectedPath) 0.999 projectedPath   

        startPoint = 
            LineSegment2d.startPoint projectedPath

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
                |> Maybe.withDefault projectedPath
                |> segmentSamplePoints
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

segmentSamplePoints : LineSegment -> List Point
segmentSamplePoints line = 
    let sampleCount = 25 in
    List.range 0 sampleCount 
        |> List.map (\i -> 
            LineSegment2d.interpolate line (toFloat i / toFloat sampleCount)
        )

type Intersection 
    = IntersectMirror MirrorBounce
    | IntersectItem Point RoomItem

tail : Sightray -> Maybe (MirrorBounce, Sightray)
tail raypath =
    case raypath.bounces of 
        [] -> Nothing 
        b :: bs ->
            { start = MirrorProjection b, bounces = bs , end = raypath.end }
                |> (\ray -> Just (b, ray))

-- all the steps in the unfolding animation 
-- from totally real at the beginning to totally projected at the end
unravel : Sightray -> List Sightray
unravel ray =
    ray :: 
        (tail ray 
            |> Maybe.map (\(bounce, tailRay) -> 
                mirrorAcross bounce.axis tailRay |> unravel
            )
            |> Maybe.withDefault []
        )
-- TODO refactor using
-- tails ray == [ ray, tail ray, tail tail ray, ... ]
-- unravel ray = tails ray |> fold (mirror each thing across the previous bounce)

vertices : Sightray -> List Point 
vertices ray = -- TODO nonempty list?
    startPos ray.start :: (List.map .point ray.bounces) ++ [ endPos ray.end ]

polyline : Sightray -> Polyline
polyline ray = 
    ray |> vertices |> Polyline2d.fromVertices

length : Sightray -> Length
length ray =
    ray |> polyline |> Polyline2d.length


interpReflect : InterpolatedReflection Sightray
interpReflect axis pct ray = 
    { ray 
        | start = updateStartPos (interpReflectPoint axis pct) ray.start 
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


-- VIEW 

view : Sightray -> Svg msg
view = 
    viewWithAttrs (lineAttrs "white" "0.03")

viewWithAttrs attrs ray = 
    ray |> polyline |> Svg.polyline2d attrs

lineAttrs : String -> String -> List (Svg.Attribute msg)
lineAttrs color width = 
    [ Attr.fill "none" 
    , Attr.stroke color
    , Attr.strokeWidth width
    , Attr.strokeDasharray "0.05"
    ]

viewSamplePoints : Sightray -> Svg msg 
viewSamplePoints ray = 
    ray 
        |> polyline
        |> Polyline2d.segments
        |> List.map segmentSamplePoints
        |> List.concat
        |> List.map (\p -> Svg.circle2d [ Attr.fill "red" ] 
            (Circle2d.atPoint p (Length.meters 0.02)))
        |> Svg.g []