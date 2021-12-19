module ScrapHeap exposing ()





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





type SightPathVertex
    = HitMirror MirrorBounce SightPathVertex
    -- | HitItem RoomItem -- TODO add this
    | StopTooFar Point

type alias MirrorBounce = 
    { axis : Axis
    , wall : LineSegment
    , point : Point
    }

nextPathVertex : Polygon2d Meters SceneCoords -> LineSegment2d Meters SceneCoords -> SightPathVertex
nextPathVertex roomShape sightline =
    let
        -- "trim" off the very beginning of the sightline by shrinking it slightly
        -- we dont want to count the start point as an intersection
        -- which will happen for all recursive calls since the sightline will start on a wall
        trimmedSightline = 
            LineSegment2d.scaleAbout (LineSegment2d.endPoint sightline) 0.999 sightline   

        mirrorBounceM : Maybe MirrorBounce
        mirrorBounceM = 
            Polygon2d.edges roomShape
                |> List.map (\e -> 
                    (LineSegment2d.intersectionPoint trimmedSightline e
                        |> Maybe.map (Tuple.pair e)))
                |> Maybe.orList
                |> Maybe.andThen (\(e, p) -> LineSegment2d.direction e |> Maybe.map (\d -> (e, p, d)))
                |> Maybe.map (\(wall, point, dir) -> 
                    { wall = wall, point = point, axis = Axis2d.withDirection dir point })
        endpoint = 
            LineSegment2d.endPoint sightline
    in
        mirrorBounceM
            |> Maybe.map (\mb -> HitMirror mb 
                (nextPathVertex roomShape (LineSegment2d.from mb.point endpoint)))
            -- TODO add this withDefaultM hitItemM
            |> Maybe.withDefault (StopTooFar endpoint)



-- compute the segment of the sightpath bouncing around the room
sightPathSegment : SightPathVertex -> LineSegment
sightPathSegment spVertex = 
    LineSegment2d.from Point2d.origin Point2d.origin -- TODO

-- compute the segment of the projected sightpath which goes straight through the mirrors
projectedPathSegment : SightPathVertex -> LineSegment
projectedPathSegment spVertex =
    sightPathSegment spVertex -- TODO


    



type alias SightPath =
    { start : Point 
    , bounces : List MirrorBounce 
    , end : Point 
    }

type alias PathSegment = 
    { real : LineSegment 
    , projected : LineSegment 
    }

startSegment : Point -> Point -> PathSegment
startSegment startP bounceP =
    let line = LineSegment2d.from startP bounceP in
    PathSegment line line

type alias NonemptyList a = List.Nonempty.Nonempty a

pathSegments : SightPath -> NonemptyList PathSegment
pathSegments path = 
    List.Nonempty.singleton (startSegment path.start path.end) -- TODO
