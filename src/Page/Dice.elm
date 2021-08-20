module Page.Dice exposing (Model, Msg(..), init, update, view)

import Bootstrap.Alert as Alert
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Dice exposing (Dice)
import Html
import Html.Attributes
import Random


type alias Model =
    { amount : String
    , faces : String
    , result : Maybe Dice.RollResult
    , resultVisibility : Alert.Visibility
    }


type Msg
    = Amount String
    | Faces String
    | Roll
    | GotRollResult Dice.RollResult


init : Model
init =
    { amount = ""
    , faces = ""
    , result = Nothing
    , resultVisibility = Alert.closed
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Amount amount ->
            ( { model | amount = amount }
            , Cmd.none
            )

        Faces faces ->
            ( { model | faces = faces }
            , Cmd.none
            )

        Roll ->
            let
                amount =
                    String.toInt model.amount
                        |> Maybe.withDefault 1

                faces =
                    String.toInt model.faces
                        |> Maybe.withDefault 20

                rollCmd =
                    Dice.roll amount (Dice.DX faces)
                        |> Random.generate GotRollResult
            in
            ( model
            , rollCmd
            )

        GotRollResult result ->
            ( { model | result = Just result, resultVisibility = Alert.shown }
            , Cmd.none
            )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Form.form []
            [ Fieldset.config
                |> Fieldset.legend [] [ Html.text "Roll XDY" ]
                |> Fieldset.children
                    [ Fieldset.config
                        |> Fieldset.asGroup
                        |> Fieldset.children
                            [ Form.label [] [ Html.text "Number of dice" ]
                            , Input.number
                                [ Input.placeholder "X"
                                , Input.value model.amount
                                , Input.onInput Amount
                                ]
                            ]
                        |> Fieldset.view
                    , Fieldset.config
                        |> Fieldset.asGroup
                        |> Fieldset.children
                            [ Form.label [] [ Html.text "Number of faces" ]
                            , Input.number
                                [ Input.placeholder "Y"
                                , Input.value model.faces
                                , Input.onInput Faces
                                ]
                            ]
                        |> Fieldset.view
                    ]
                |> Fieldset.view
            , Button.button
                [ Button.primary
                , Button.attrs
                    [ Html.Attributes.type_ "button"
                    ]
                , Button.onClick Roll
                ]
                [ Html.text "Roll" ]
            ]
        , Html.div
            []
            [ Alert.config
                |> Alert.success
                |> Alert.children
                    [ Alert.h4 [] [ Html.text "Result" ]
                    , Html.text
                        (case model.result of
                            Nothing ->
                                "None"

                            Just result ->
                                result.description ++ ": " ++ String.fromInt result.value
                        )
                    ]
                |> Alert.view model.resultVisibility
            ]
        ]
