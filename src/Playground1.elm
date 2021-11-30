module Playground1 exposing (..)

import Angle
import Browser
import Circle2d
import Frame2d exposing (Frame2d)
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Quantity
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Element as El exposing (Element)
import Element.Border as Border
import Vector2d exposing (Vector2d)
import Pixels exposing (Pixels)

main = 
    Browser.sandbox
        { init = init
        , update = update   
        , view = view
        }
        


type TopLeftCoords = TopLeftCoords 

type alias Model = 
    { angle : Float 
    , catFrame : Frame2d Pixels TopLeftCoords {}
    }

init = 
    { angle = 25 
    , catFrame = 
        Frame2d.atOrigin
            |> Frame2d.translateBy (Vector2d.pixels 50 50)
            |> Frame2d.rotateBy (Angle.degrees 65)
    }

        
update msg model = 
    model     
        
constants =
    { }

view : Model -> Html ()
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


svgContainer model =
    Svg.svg 
        [ Attr.width "500"
        , Attr.height "500"
        ]
        [ diagram model ]

diagram : Model -> Svg ()
diagram model =
    let 
        catLocation = 
            Frame2d.originPoint model.catFrame

        watchDistance = Pixels.float 350

        mirrorLine =
            LineSegment2d.from 
                (Point2d.pixels 0 100)
                (Point2d.pixels 300 100)
        sightLine = 
            LineSegment2d.fromPointAndVector catLocation
                (Vector2d.withLength watchDistance (Frame2d.xDirection model.catFrame))
    in
    Svg.g 
        []
        [ Svg.lineSegment2d 
            [ Attr.stroke "blue" 
            , Attr.width "2"
            ]
            sightLine
        , Svg.lineSegment2d
            [ Attr.stroke "black" 
            , Attr.width "2"
            ]
            mirrorLine
        , LineSegment2d.intersectionPoint sightLine mirrorLine
            |> Maybe.map (\p -> 
                Svg.circle2d [ Attr.fill "yellow" ]
                    (Circle2d.atPoint p (Pixels.float 2))
            )
            |> Maybe.withDefault svgEmtpy
        , Svg.circle2d [ Attr.fill "purple" ]
            (Circle2d.atPoint catLocation (Pixels.float 4))
        ]

svgEmtpy = Svg.g [] []