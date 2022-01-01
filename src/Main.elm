module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Diagram 
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import List.Extra as List
import Maybe.Extra as Maybe
import Quantity
import Shared
import Shared exposing (noCmds)
import Browser.Dom
import Task


main = 
    Browser.element
        { init = init
        , update = update   
        , subscriptions = subscriptions
        , view = view
        }

type alias Model =
    { levelIndex : Int
    , levels : List Diagram.Model
    , device : El.Device
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( { levelIndex = 0
      , levels = initialLevels
      , device = El.classifyDevice { width = 800, height = 600 }
      }
    , Task.perform 
        (\vp -> WindowResize (round vp.scene.width) (round vp.scene.height)) 
        Browser.Dom.getViewport
    )

initialLevels : List Diagram.Model 
initialLevels = 
    [ Diagram.initLevel1
    , Diagram.initLevel2
    , Diagram.initLevel3
    , Diagram.initLevel4 
    ]

diagramM : Model -> Maybe Diagram.Model
diagramM model = 
    model.levels 
        |> List.getAt model.levelIndex

diagram =
    diagramM >> Maybe.withDefault Diagram.initLevel1


-- SUBSCRIPTIONS -- 

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ diagram model |> Diagram.subscriptions |> Sub.map DiagramMsg
        , Browser.Events.onResize WindowResize
        ]


-- UPDATE --

type Msg 
    = NoOp
    | DiagramMsg Diagram.Msg
    | ChangeLevel Int
    | Reset Int
    | WindowResize Int Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        DiagramMsg dm -> 
            model 
                |> updateDiagramAtIndex model.levelIndex (Diagram.update dm)
                |> Shared.noCmds

        ChangeLevel amount ->
            { model | levelIndex = 
                model.levelIndex + amount 
                    |> clamp 0 (List.length model.levels - 1) 
            }
                |> Shared.noCmds

        Reset index -> 
            model 
                |> updateDiagramAtIndex index (resetDiagramIfSucceeded index)
                |> Shared.noCmds

        WindowResize width height ->
            { model | device = El.classifyDevice { width = width, height = height } 
                |> Shared.debugLogF "device and width" (Tuple.pair width) }
                |> noCmds

        _ ->
            model 
                |> Shared.noCmds

updateDiagramAtIndex : Int -> (Diagram.Model -> Diagram.Model) -> Model -> Model 
updateDiagramAtIndex index upd model =
    { model | levels = model.levels 
        |> List.indexedMap (\i dia -> if i == index then upd dia else dia)
    }

resetDiagramIfSucceeded : Int -> Diagram.Model -> Diagram.Model 
resetDiagramIfSucceeded index dia = 
    List.getAt index initialLevels 
        |> Maybe.filter (\_ -> Diagram.hasSucceeded dia)
        |> Maybe.withDefault dia

-- VIEW --

view : Model -> Html Msg
view model = 
    El.layout 
        [ El.width (El.fill)
        , El.height El.fill 
        , El.centerX
        ]
        (El.el 
            [ El.width (El.fill |> El.maximum 700)
            , El.centerX 
            ] 
            (El.column 
                [ El.centerX 
                , case model.device.class of 
                    El.Phone -> El.paddingXY 0 0
                    _ -> El.paddingXY 20 20
                , El.spacing 50
                , Font.size 16
                , Font.family 
                    [ Font.external
                        { name = "Cantarell"
                        , url = "https://fonts.googleapis.com/css?family=Cantarell"
                        }
                    ]
                , Font.color darkGrey
                ]
                [ El.column [ El.padding 5, El.spacing 50 ] 
                    [ El.el [ Font.size 30 ] 
                        <| El.text "Infinite Forest Box"
                    , viewScenario
                    ]
                , viewDiagramContainer model
                ]
            )
        )

viewScenario : Element Msg 
viewScenario = 
    El.column 
        [ El.spacing 30 ] 
        [ El.el [ Font.size 22 ] <| El.text "Scenario"
        , El.paragraph paragraphAttrs 
            [ El.text "Pat the Cat 🐈‍ is an avid wildlife photographer. "
            , El.text "She recently bought a fancy new camera, "
            , El.text "and is excited to start taking some pics. "
            , El.text "She is especially curious to test out the range "
            , El.text "of its zoom capabilities."
            ]
        , El.paragraph paragraphAttrs
            [ El.text "Arriving home, Pat enters her "
            , El.el [ ] (El.text "Infinite Forest Box, ")
            , El.text "a small room with 4 adjustable mirrors for walls. "
            , El.text "The room doesn't contain much, just Garrett the Parrot 🦜 "
            , El.text "and a few potted plants 🌱. "
            , El.text "But the light bouncing around off the mirrored walls "
            , El.text """gives Pat the illusion of standing in an "infinite forest" """
            , El.text "surrounded by many plants and birds: "
            , El.text " some close by, and others far away..."
            ]
        ]

viewDiagramContainer : Model -> Element Msg
viewDiagramContainer model = 
    El.column     
        [ El.centerX 
        , El.width El.fill
        , Border.width 2
        , Border.rounded 4
        , Border.color veryLightGrey
        , El.htmlAttribute (Html.Attributes.style "touch-action" "none")
        ] 
        [ viewLevelControls model.levels model.levelIndex
        , El.column [ El.padding 10, El.width El.fill ]  
            [ instructionsParagraph model.levelIndex (diagram model)
            , El.el 
                [ El.centerX ] 
                ( diagram model 
                    |> Diagram.view 
                    |> El.html 
                    |> El.map DiagramMsg
                )
            ]
        ]

darkGrey = El.rgb 0.35 0.35 0.35 
lightGrey = El.rgb 0.7 0.7 0.7
veryLightGrey = El.rgb 0.9 0.9 0.9
yellow1 = El.rgb 0.85 0.65 0.4

viewLevelControls : List Diagram.Model -> Int -> Element Msg 
viewLevelControls levels levelIndex = 
    El.el 
        [ Font.size 20
        , Font.bold
        , El.centerX
        , El.paddingXY 15 15
        , El.width El.fill
        , Background.color veryLightGrey 
        , Font.color darkGrey
        ]
        <| El.row 
            [ El.spacing 20, El.centerX ] 
            [ viewLevelButton levels levelIndex "<" -1
            , levelIndex |> (+) 1 |> String.fromInt |> (++) "Level " |> El.text 
            , viewLevelButton levels levelIndex ">" 1
            ]

viewLevelButton : List a -> Int -> String -> Int -> Element Msg
viewLevelButton levels levelIndex labelText changeAmount = 
    let 
        onPress = 
            levels 
                |> List.getAt (levelIndex + changeAmount)
                |> Maybe.map (\_ -> ChangeLevel changeAmount)
        
        enabled = 
            Maybe.isJust onPress

        attrs = 
            [ El.focused []
            , El.alpha <| if enabled then 1 else 0.3
            , El.mouseDown <| if enabled then [ Font.color veryLightGrey ] else []
            ]
    in
    Input.button attrs 
        { label = El.text labelText, onPress = onPress }

paragraphAttrs =
    [ El.spacing 15 ] 


instructionsParagraph : Int -> Diagram.Model -> Element Msg
instructionsParagraph levelIndex level = 
    let
        body = 
            if Diagram.hasSucceeded level then
                [ El.text "You did it! " 
                , Input.button 
                    [ Font.color yellow1 , Font.bold ]
                    { label = El.text "Try again"
                    , onPress = Just (Reset levelIndex)
                    }
                ]
            else 
                [ El.paragraph [ El.spacing 15 ]
                    [ El.text "Aim the cat's camera to take a photo of a version " 
                    , El.text "of the bird in the mirror that appears to be "
                    , El.el [ Font.bold, Font.color yellow1 ] 
                        (El.text <| String.fromFloat (Quantity.unwrap level.sightDistance) ++ " meters away.")
                    ]
                ]
    in
    
    El.row
        [ El.paddingXY 40 40
        , El.centerX
        , El.spacing 5
        ]
        body

