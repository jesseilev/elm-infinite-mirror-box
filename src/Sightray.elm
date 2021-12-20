module Sightray exposing (..)

import Axis2d
import LineSegment2d exposing (LineSegment2d)
import List.Nonempty
import Maybe.Extra as Maybe
import Point2d
import Polygon2d
import RoomItem exposing (RoomItem)
import Shared exposing (..)

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
    | HitItem RoomItem

type alias MirrorBounce = 
    { axis : Axis
    , wall : LineSegment
    , point : Point
    }



-- Constructors -- 

noBounces : RayStart -> RayEnd -> Sightray
noBounces start end = 
    Sightray start [] end

fromRoomAndProjectedPath : Polygon -> LineSegment -> Sightray
fromRoomAndProjectedPath roomShape projectedPath = 
    let 
        (projectedStart, projectedEnd) =
            LineSegment2d.endpoints projectedPath

        nextProjectedPath bounce =
            LineSegment2d.from bounce.point projectedEnd
                |> LineSegment2d.mirrorAcross bounce.axis

        recurse bounce = 
            fromRoomAndProjectedPath roomShape (nextProjectedPath bounce)
                |> (\ray -> { ray | start = updateStartPos (\_ -> projectedStart) ray.start })
                |> addBounce bounce
    in
    nextBounce roomShape projectedPath 
        |> Maybe.map recurse
        |> Maybe.withDefault 
            (noBounces (StartAtPlayer (LineSegment2d.startPoint projectedPath)) (TooFar projectedEnd))
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
        HitItem item -> HitItem (RoomItem.setPos newPos item)

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
        HitItem i -> i.pos

nextBounce : Polygon -> LineSegment -> Maybe MirrorBounce
nextBounce roomShape projectedPath =
    let
        -- "trim" off the very beginning of the sightline by shrinking it slightly
        -- we dont want to count the start point as an intersection
        -- which will happen for all recursive calls since the sightline will start on a wall
        trimmedSightline = 
            LineSegment2d.scaleAbout (LineSegment2d.endPoint projectedPath) 0.999 projectedPath    
    in
    Polygon2d.edges roomShape
        |> List.map (\e -> 
            (LineSegment2d.intersectionPoint trimmedSightline e
                |> Maybe.map (Tuple.pair e)))
        |> Maybe.orList
        |> Maybe.andThen (\(e, p) -> LineSegment2d.direction e |> Maybe.map (\d -> (e, p, d)))
        |> Maybe.map (\(wall, point, dir) -> 
            { wall = wall, point = point, axis = Axis2d.withDirection dir point })

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
                mirrorAcross bounce.axis tailRay
                    |> unravel
            )
            |> Maybe.withDefault []
        )
-- TODO refactor using
-- tails ray == [ ray, tail ray, tail tail ray, ... ]
-- unravel ray = tails ray |> fold (mirror each thing across the previous bounce)

vertices : Sightray -> List Point 
vertices ray = -- TODO nonempty list?
    startPos ray.start :: (List.map .point ray.bounces) ++ [ endPos ray.end ]


-- interpReflect : InterpolatedReflection Sightray
-- interpReflect axis pct ray = 
--     { ray 
--         | start = ray.start 
--         , bounces = List.map interpReflectBounce
--     }