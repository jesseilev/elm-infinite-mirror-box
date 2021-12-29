module ScrapHeap exposing ()
import Sightray





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




-- nextMirrorBounce : LineSegment -> Polygon -> Maybe MirrorBounce
-- nextMirrorBounce sightline roomShape =
--     let
--         -- "trim" off the very beginning of the sightline by shrinking it slightly
--         -- we dont want to count the start point as an intersection
--         -- which will happen for all recursive calls since the sightline will start on a wall
--         trimmedSightline = 
--             LineSegment2d.scaleAbout (LineSegment2d.endPoint sightline) 0.999 sightline    
--     in
--     Polygon2d.edges roomShape
--         |> List.map (\e -> 
--             (LineSegment2d.intersectionPoint trimmedSightline e
--                 |> Maybe.map (Tuple.pair e)))
--         |> Maybe.orList
--         |> Maybe.andThen (\(e, p) -> LineSegment2d.direction e |> Maybe.map (\d -> (e, p, d)))
--         |> Maybe.map (\(wall, point, dir) -> 
--             { wall = wall, point = point, axis = Axis2d.withDirection dir point })

-- -- TODO remake this using Room type
-- reflectedRooms : LineSegment -> Polygon -> List Polygon -> List Polygon
-- reflectedRooms sightline room roomsAcc = 
--     nextMirrorBounce sightline room
--         |> Maybe.map (\inter -> 
--             reflectedRooms 
--                 (LineSegment2d.from inter.point (LineSegment2d.endPoint sightline))
--                 (room |> Polygon2d.mirrorAcross inter.axis)
--                 (room :: roomsAcc))
--         |> Maybe.withDefault (room :: roomsAcc)



-- reflectedRooms : Model -> Shared.SuccessAnimation -> List Room.Model
-- reflectedRooms model animation =
--     rayNormal model
--         |> .bounces 
--         |> List.inits
--         |> List.map (\bounces -> reflectedRoom bounces model.room)


-- viewDiagramSuccess model animation = 
--     reflectionHallway model animation
--         |> List.map (\(room, ray) -> viewRoomWithRay room ray)
--         |> Svg.g []
--         |> Svg.at (Shared.pixelsPerMeter 0.3)--(0.6 ^ toFloat animation.step))
--         |> Svg.relativeTo Shared.topLeftFrame

-- reflectionHallway : Model -> Shared.SuccessAnimation -> Hallway
-- reflectionHallway model ani =


-- type alias Hallway = 
--     { closeRoom : Room
--     , farRoom : Room
--     , farRoomRay : Sightray
--     }

-- viewRoomWithRay : Room.Model -> Sightray -> Svg Msg
-- viewRoomWithRay room ray =
--     Svg.g [] [] -- TODO



viewRaySuccess : Model -> SuccessAnimation -> Svg Msg
viewRaySuccess model animation =
    let
        -- (successRay, centerPoint) = 
        --     rayNormal model
        --         |> Sightray.unravel
        --         |> Array.fromList
        --         |> (\rs -> Array.get animation.step rs)
        --         |> Maybe.map (\r -> 
        --             case Sightray.tail r of 
        --                 Nothing -> (r, Sightray.startPos r.start)
        --                 Just (bounce, tail) ->
        --                     ( tail 
        --                         -- |> Sightray.interpReflect bounce.axis 
        --                         --     (animation.transitionPct |> Maybe.withDefault 0)
        --                     , bounce.point
        --                     )
        --         )
        --             -- ( { r | start = Sightray.updateStartPos (\_ -> model.room.viewerPos) r.start }
        --             -- , Sightray.startPos r.start
        --             -- ))
        --         |> Maybe.withDefault (rayNormal model, model.room.viewerPos)  
        
        lineAttrs color = 
            [ Attr.fill "none" 
            , Attr.stroke color
            , Attr.strokeWidth "0.03"
            , Attr.strokeDasharray "0.05"
            ]

        raySucc = 
            raySuccess model animation 
    in
    raySucc
        |> Sightray.vertices
        |> Polyline2d.fromVertices
        |> (\poly -> Svg.g [] 
            [ Svg.polyline2d (lineAttrs "black") poly 
            , Svg.lineSegment2d (lineAttrs "red") 
                (LineSegment2d.from model.room.viewerPos (Sightray.startPos raySucc.start))
            ])





-- viewReflectedRooms : Model -> Svg Msg 
-- viewReflectedRooms model = 
--     let
--         reflectedRoom mirrorWall = 
--             model.roomShape 
--                 |> Polygon2d.mirrorAcross mirrorWall.axis
        
--         wallM = 
--             Polygon2d.edges model.roomShape
--                 |> List.head
--                 |> Maybe.andThen mkWall

--         mkWall : LineSegment -> Maybe { wall : LineSegment, axis : Axis }
--         mkWall line = 
--             LineSegment2d.direction line
--                 |> Maybe.map (Axis2d.through (LineSegment2d.startPoint line))
--                 |> Maybe.map (\a -> { wall = line, axis = a})
--     in
--         reflectedRooms (projectedSightline model) model.roomShape []
--             |> List.map (Svg.polygon2d 
--                 [ Attr.fill "none"
--                 , Attr.stroke "grey"
--                 , Attr.strokeWidth "0.02" 
--                 ])
--             |> Svg.g [] 



-- viewDebugStuff : Model -> Svg Msg 
-- viewDebugStuff model = 
--     Svg.g []
--         [ frameDebugViz "purple"
--             |> Svg.placeIn (roomFrame model)
--         , frameDebugViz "orange" 
--             |> Svg.placeIn (viewerFrame model)
--         -- , frameDebugViz "grey"
--         --     |> Svg.relativeTo ((topLeftFrame model) |> Frame2d.translateBy (Vector2d.pixels -100 -100))
--         , Svg.circle2d
--             [ Attr.fill "red" ]
--             (Circle2d.atPoint model.clickPosDebug (Length.meters 0.05))
--         ]



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





type ApparentRoom 
    = ActualRoom Room.Model
    | ReflectionOf ApparentRoom Sightray.MirrorBounce

computeApparentRoom : ApparentRoom -> Room.Model 
computeApparentRoom ar =
    case ar of 
        ActualRoom room -> room
        ReflectionOf twin bounce ->
            computeApparentRoom twin
                |> Room.interpReflect bounce.axis 1

apparentHallway : ApparentRoom -> List Room.Model 
apparentHallway ar = 
    case ar of 
        ActualRoom room -> [ room ]
        ReflectionOf twin _ ->
            computeApparentRoom ar :: apparentHallway twin

lastApparentRoom : Room.Model -> List Sightray.MirrorBounce -> ApparentRoom
lastApparentRoom room bounces = 
    case bounces of 
        [] -> ActualRoom room
        b :: bs -> 
            ReflectionOf (lastApparentRoom room bs) b

bouncesToHallway room bounces = 
    lastApparentRoom room bounces
        |> apparentHallway



flipRoom : Sightray.MirrorBounce -> Room.Model -> Room.Model
flipRoom bounce =
    Room.interpReflect bounce.axis 1


reflectedRoom bounces room =
    case bounces of
        [] -> room
        b :: bs ->
            room 
                |> Room.interpReflect b.axis 1
                |> reflectedRoom bs
                



type alias Ray = 
    { start : Point 
    , unfoldedBounces : List Sightray.MirrorBounce
    , foldedBounces : List Sightray.MirrorBounce
    , end : Sightray.RayEnd
    }


{- 
I want to calculate the reflected rooms and the sightray at any step in the (un)raveling process
inputs:
    - roomshape
    - player pos
    - player direction
    - sight distance
    - room item positions
    - unravelStep

I want the angle of a given bounce
I want the corresponding (un)raveled twin of a given bounce
i want the previous point before a given boucne
i want the next point after a given bounce


At the very end i want these features:
I want to render the success animation
To render the sucess animation I need to compute the reflected rooms and the sightray


maybe i want to store the sightray as a tree structure
where only one path through the tree is visible at a particular time

i think a good foundational thing to compute would be:
a function from current unravel step to Sightray
or a list of Sightray, one for each step
but dont i already have this?
Sightray.unravel gives a list
but it shortens the list instead of just updating the mirrorintersection types
so redefine sightray to includ ethe projection points

-}

hallway : Sightray -> Room -> List Room
hallway ray room = 
    ray.bounces 
        |> List.map (\bounce -> 
            Room.mirrorAcross bounce.axis
        )



type Intersection 
    = IntersectMirror MirrorIntersection
    | IntersectItem ItemIntersection

type MirrorIntersection
    = MIReflect Sightray.MirrorBounce
    | MIProject Sightray.MirrorBounce

type alias ItemIntersection = 
    { item : RoomItem 
    , endpoint : Point 
    }

type RayEnd_ 
    = RETooFar Point 
    | REItem ItemIntersection


type MirrorHit 
    = Projection RayVertex
    | Bounce RayVertex

type RayVertex 
    = MH MirrorHit
    | RE RayEnd

