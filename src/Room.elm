module Room exposing 
    ( allItems
    , level1
    , level2
    , level3
    , level4
    , interpolateFrom
    , mirrorAcross
    , playerItem
    , Room
    , targetItem
    , view
    )

import Angle
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Length exposing (Meters)
import Quantity exposing (Quantity)
import Pixels exposing (Pixels, pixels)
import Point2d exposing (Point2d)
import Polygon2d
import RoomItem exposing (RoomItem)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import TypedSvg.Types exposing (CoordinateSystem(..), Paint(..))
import Shared exposing(Axis, Polygon)
import Direction2d exposing (Direction2d)
import Polyline2d exposing (Polyline2d)


-- Types --

type alias Room = 
    { wallShape : Polygon
    , playerItem : RoomItem
    , targetItem : RoomItem
    , trees : List RoomItem
    }

 -- Constructors --

level1 : Room 
level1 = 
    { wallShape = 
        Polygon2d.singleLoop 
            [ Point2d.meters -2.0 -1.5
            , Point2d.meters 1.5 -2.1
            , Point2d.meters 2.0 1.0
            , Point2d.meters -2.0 2.0
            ]
    , playerItem = RoomItem (Point2d.meters 0.85 0.8) RoomItem.emojis.cat
    , targetItem = RoomItem (Point2d.meters -0.5 -0.7) RoomItem.emojis.parrot
    , trees = []
    }

level2 : Room 
level2 =
    { wallShape = 
        Polygon2d.singleLoop 
            [ Point2d.meters -1.75 -1.25
            , Point2d.meters 2.1 -2.0
            , Point2d.meters 1.5 1.25
            , Point2d.meters -1.5 2.1
            ]
    , playerItem = RoomItem (Point2d.meters -0.65 -0.9) RoomItem.emojis.cat
    , targetItem = RoomItem (Point2d.meters 0.7 -0.6) RoomItem.emojis.parrot
    , trees =
        [ RoomItem (Point2d.meters 0.3 -1.4) RoomItem.emojis.plant
        , RoomItem (Point2d.meters 0.7 0.1) RoomItem.emojis.plant
        ]
    }

level3 : Room 
level3 =
    { wallShape = 
        Polygon2d.singleLoop 
            [ Point2d.meters -2.1 -2.0
            , Point2d.meters 1.8 -1.2
            , Point2d.meters 2.05 1.0
            , Point2d.meters -1.5 2.0
            ]
    , playerItem = RoomItem (Point2d.meters 0.35 -0.65) RoomItem.emojis.cat
    , targetItem = RoomItem (Point2d.meters 1.3 0.1) RoomItem.emojis.parrot
    , trees =
        [ RoomItem (Point2d.meters -0.5 0.3) RoomItem.emojis.plant
        , RoomItem (Point2d.meters 0.2 0.9) RoomItem.emojis.plant
        , RoomItem (Point2d.meters 1.1 -0.4) RoomItem.emojis.plant
        ]
    }

level4 : Room 
level4 =
    { wallShape = 
        Polygon2d.singleLoop 
            [ Point2d.meters -2.0 -2.0
            , Point2d.meters 1.8 -1.8
            , Point2d.meters 1.9 2.0
            , Point2d.meters -1.75 1.0
            ]
    , playerItem = RoomItem (Point2d.meters 1.35 0.5) RoomItem.emojis.cat
    , targetItem = RoomItem (Point2d.meters -0.65 0.55) RoomItem.emojis.parrot
    , trees = []
    }


-- Properties -- 

playerItem : Room -> RoomItem 
playerItem = 
    .playerItem
    -- ( case model.status of 
    --     Standing -> RoomItem.emojis.cat
    --     LookingAround -> RoomItem.emojis.cameraVid2 
    --     TakingPic -> RoomItem.emojis.cameraFlash
    -- )
        -- |> 
        -- RoomItem.init model.viewerPos emoji


targetItem : Room -> RoomItem 
targetItem = 
    .targetItem 


allItems : Room -> List RoomItem
allItems model = 
    model.trees ++ [ model.targetItem, model.playerItem ]


-- Transformations --

mirrorAcross : Axis -> Room -> Room 
mirrorAcross axis room = 
    { wallShape = Polygon2d.mirrorAcross axis room.wallShape 
    , playerItem = RoomItem.mirrorAcross axis room.playerItem 
    , targetItem = RoomItem.mirrorAcross axis room.targetItem 
    , trees = List.map (RoomItem.mirrorAcross axis) room.trees 
    }

interpolateFrom : Room -> Room -> Float -> Room
interpolateFrom r1 r2 pct = 
    { wallShape = Shared.interpolatePolygonFrom r1.wallShape r2.wallShape pct
    , playerItem = RoomItem.interpolateFrom r1.playerItem r2.playerItem pct
    , targetItem = RoomItem.interpolateFrom r1.targetItem r2.targetItem pct
    , trees = Shared.interpolateLists RoomItem.interpolateFrom r1.trees r2.trees pct
    }

-- VIEW

view : Float -> Room -> Svg msg 
view zoomScale model = 
    let 
        viewRoomItem item = 
            RoomItem.create item.pos item.emoji
                |> RoomItem.view

        roomSvg =
            Svg.g [] <| 
                [ Svg.polygon2d 
                    [ Attr.fill "none"
                    , Attr.strokeWidth <| Shared.floatAttributeForZoom zoomScale 0.03
                    , Attr.stroke "lightGrey"
                    -- , Attr.opacity "0.5"
                    ]
                    (model.wallShape |> Polygon2d.placeIn Shared.roomFrame)
                , viewRoomItem model.playerItem
                    -- |> Svg.rotateAround model.viewerPos 
                    --     (if model.status == Standing then 
                    --         Angle.degrees 0
                    --     else 
                    --      Direction2d.toAngle model.viewerDirection |> Quantity.minus (Angle.degrees 90)
                    --     )
                , viewRoomItem model.targetItem
                ]
                ++ (List.map viewRoomItem model.trees)        
    in
        roomSvg