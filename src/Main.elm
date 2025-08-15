module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import HistogramChart as HistogramChart
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Material.IconButton as IconButton
import Material.List as MList
import Material.List.Item as ListItem
import Material.Typography as Typography
import Random
import Stat as Stat
import StatRandom as StatRandom
import Svg exposing (view)


type Estimate
    = Estimate
        { id : Int
        , description : String
        , min : Float
        , max : Float
        }


emptyEstimate : Model -> Estimate
emptyEstimate m =
    Estimate
        { id = m.idCounter + 1
        , description = ""
        , min = 0.0
        , max = 10.0
        }


voidEstimate : Estimate
voidEstimate =
    Estimate
        { id = 0
        , description = ""
        , min = 0.0
        , max = 10.0
        }


type alias Model =
    { idCounter : Int
    , estimates : List Estimate
    , samples : List Float
    }


initialModel : Model
initialModel =
    Model 0 [ voidEstimate ] []


type Msg
    = AddEmptyEstimate
    | DeleteEstimate String
    | ChangeDescription Int String
    | ChangeMin Int String
    | ChangeMax Int String
    | NormalList (List Float)


type UpdateEstimate
    = UpdateDescription
    | UpdateMin
    | UpdateMax


getNewEstimates : Int -> String -> Model -> UpdateEstimate -> List Estimate
getNewEstimates n v m t =
    let
        ( first, prevEstimates ) =
            List.partition (\(Estimate x) -> x.id == n) <|
                m.estimates

        (Estimate selectedEstimate) =
            Maybe.withDefault voidEstimate
                << List.head
            <|
                first

        updatedEstimate =
            case t of
                UpdateDescription ->
                    { selectedEstimate | description = v }

                UpdateMin ->
                    { selectedEstimate | min = Maybe.withDefault 0.0 << String.toFloat <| v }

                UpdateMax ->
                    { selectedEstimate | max = Maybe.withDefault 0.0 << String.toFloat <| v }
    in
    List.sortBy (\(Estimate e) -> e.id) <|
        Estimate updatedEstimate
            :: prevEstimates


monteCarloSamples : Int -> Float -> Float -> Int -> Random.Generator (List Float)
monteCarloSamples numSamples a b seed =
    let
        intervalLength =
            b - a

        samplesPerIteration =
            numSamples // 1000

        iterations =
            numSamples // samplesPerIteration

        lower =
            -1.0

        upper =
            1.0

        midpoint =
            (lower + upper) / 2.0

        generator =
            StatRandom.normal 0 1

        sampleFunction x =
            (intervalLength * x) + a

        acceptSample x =
            let
                y =
                    -- Random.generate generator seed |> Tuple.second
                    Random.generate generator seed |> Tuple.second
            in
            if (y < (1.0 / (sqrt 2.0 * pi))) * Basics.e ^ ((-1.0 * (x - midpoint) * (x - midpoint)) / 2.0) then
                True

            else
                False

        monteCarloIteration acc _ =
            let
                acceptedSamples =
                    List.range 1 samplesPerIteration
                        |> List.map (\_ -> Random.float lower upper |> acceptSample |> sampleFunction)
            in
            acceptedSamples :: acc
    in
    Random.list iterations (monteCarloIteration []) |> Random.map List.concat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddEmptyEstimate ->
            ( { model
                | idCounter = model.idCounter + 1
                , estimates = model.estimates ++ [ emptyEstimate model ]
              }
            , Random.generate NormalList (StatRandom.generateList 1000 (StatRandom.normal 2 0.5))
            )

        NormalList list ->
            ( { model | samples = list }
            , Cmd.none
            )

        DeleteEstimate id ->
            ( { model
                | estimates = List.filter (\(Estimate x) -> x.id /= Maybe.withDefault 0 (String.toInt id)) model.estimates
              }
            , Cmd.none
            )

        ChangeDescription estId desc ->
            ( { model
                | estimates = getNewEstimates estId desc model UpdateDescription
              }
            , Cmd.none
            )

        ChangeMax estId m ->
            ( { model
                | estimates = getNewEstimates estId m model UpdateMax
              }
            , Cmd.none
            )

        ChangeMin estId m ->
            ( { model
                | estimates = getNewEstimates estId m model UpdateMin
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        firstEstimate =
            ListItem.listItem ListItem.config <|
                [ (viewEstimate << Maybe.withDefault voidEstimate) (List.head model.estimates) ]

        tails =
            Maybe.withDefault [] <| List.tail model.estimates

        tailEstimates =
            List.map ((\e -> ListItem.listItem ListItem.config [ e ]) << viewEstimate) tails

        mainList =
            MList.list MList.config firstEstimate tailEstimates
    in
    Element.layout
        [ Font.size 16
        , Font.family
            [ Font.typeface "Helvetica"
            , Font.sansSerif
            ]
        , Element.height fill
        , htmlAttribute <| Typography.typography
        ]
    <|
        Element.row
            []
            [ Element.column [ Element.alignTop ] [ addButton |> Element.html, mainList |> Element.html ]
            , normal model
            ]


box : String -> List (Element Msg) -> Element Msg
box id_ cc =
    el [ Element.alignTop, Element.alignRight, Element.width (px 800), Element.height shrink, Border.width 1, Border.color black, Border.rounded 5, Border.shadow { offset = ( 0, 0 ), size = 10.0, blur = 50.0, color = black }, centerX, padding 30 ]
        (column [ htmlAttribute (id id_) ]
            cc
        )


black : Color
black =
    Element.rgba 0 0 0 0.1


descriptionTitle : String -> Element Msg
descriptionTitle title =
    el [ Font.size 24, paddingXY 0 30, Font.underline ] (Element.text title)


normal : Model -> Element Msg
normal model =
    box "Normal"
        [ descriptionTitle "Monte Carlo Sampled Distribution"
        , el [ Element.width <| px 750, Element.height fill ] (HistogramChart.view model.samples ( 0, 4 ) 20 20 |> Element.html)
        ]


addButton : Html Msg
addButton =
    IconButton.iconButton
        (IconButton.config
            |> IconButton.setOnClick AddEmptyEstimate
            |> IconButton.setAttributes
                [ style "margin" "15px 160px"
                , style "border-radius" "50px"
                , class "mdc-button--unelevated"
                , style "color" "white"
                ]
        )
        (IconButton.icon "add")


deleteButton : Estimate -> Html Msg
deleteButton (Estimate e) =
    IconButton.iconButton
        (IconButton.config
            |> IconButton.setOnClick (DeleteEstimate (String.fromInt e.id))
            |> IconButton.setAttributes
                [ style "margin" "0px 0px"
                , style "border-radius" "5px"
                ]
        )
        (IconButton.icon "delete")


viewEstimate : Estimate -> Html Msg
viewEstimate ((Estimate e) as est) =
    div []
        [ input [ placeholder "Label", value e.description, onInput (ChangeDescription e.id) ] []
        , Html.text "min"
        , input [ value (String.fromFloat e.min), onInput (ChangeMin e.id) ] []
        , Html.text "max"
        , input [ value (String.fromFloat e.max), onInput (ChangeMax e.id) ] []
        , deleteButton est
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
