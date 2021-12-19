module RayPath exposing (..)

import Axis2d
import LineSegment2d exposing (LineSegment2d)
import List.Nonempty
import Maybe.Extra as Maybe
import Point2d
import Polygon2d
import Shared exposing (..)

type alias RayPath =
    { start : Point 
    , bounces : List MirrorBounce 
    , end : Point 
    }

type alias MirrorBounce = 
    { axis : Axis
    , wall : LineSegment
    , point : Point
    }


-- Constructors -- 

noBounces : Point -> Point -> RayPath
noBounces start end = 
    RayPath start [] end

fromRoomAndProjectedPath : Polygon -> LineSegment -> RayPath
fromRoomAndProjectedPath roomShape projectedPath = 
    let 
        (projectedStart, projectedEnd) =
            LineSegment2d.endpoints projectedPath

        nextProjectedPath bounce =
            LineSegment2d.from bounce.point projectedEnd
                |> LineSegment2d.mirrorAcross bounce.axis

        recurse bounce = 
            fromRoomAndProjectedPath roomShape (nextProjectedPath bounce)
                |> (\rp -> { rp | start = projectedStart })
                |> addBounce bounce
    in
    nextBounce roomShape projectedPath 
        |> Maybe.map recurse
        |> Maybe.withDefault 
            (noBounces (LineSegment2d.startPoint projectedPath) projectedEnd)
 

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


 -- Transformations --

addBounce : MirrorBounce -> RayPath -> RayPath
addBounce bounce rp =
    { rp | bounces = bounce :: rp.bounces }

prependStart : Point -> RayPath -> RayPath
prependStart start rp = 
    { rp | start = start }
        -- |> addBounce rp.start

mirrorAcross : Axis -> RayPath -> RayPath
mirrorAcross axis raypath =
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
    { start = reflectPoint raypath.start 
    , bounces = List.map reflectBounce raypath.bounces
    , end = reflectPoint raypath.end 
    }


tail : RayPath -> Maybe (MirrorBounce, RayPath)
tail raypath =
    case raypath.bounces of 
        [] -> Nothing 
        b :: bs ->
            Just ( b, { start = b.point , bounces = bs , end = raypath.end } )

-- all the steps in the unfolding animation 
-- from totally real at the beginning to totally projected at the end
unfold : RayPath -> List RayPath
unfold raypath =
    raypath :: 
        (tail raypath 
            |> Maybe.map (\(b, rp) -> 
                mirrorAcross b.axis rp
                    |> unfold
            )
            |> Maybe.withDefault []
        )


-- Properties --

vertices : RayPath -> List Point 
vertices raypath = -- TODO nonempty list?
    raypath.start :: (List.map .point raypath.bounces) ++ [ raypath.end ]


type alias PathSegment = 
    { real : LineSegment 
    , projected : LineSegment 
    }

startSegment : Point -> Point -> PathSegment
startSegment startP bounceP =
    let line = LineSegment2d.from startP bounceP in
    PathSegment line line

type alias NonemptyList a = List.Nonempty.Nonempty a

pathSegments : RayPath -> NonemptyList PathSegment
pathSegments path = 
    List.Nonempty.singleton (startSegment path.start path.end) -- TODO
