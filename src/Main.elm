module Main exposing (main)

import Base
import Bootstrap.Navbar as Navbar
import Browser
import Browser.Navigation
import Html
import Html.Attributes
import Octicons
import Page.Dice
import Page.Home
import Url
import Url.Builder
import Url.Parser exposing ((</>))



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Page
    = Home
    | Dice
    | NotFound


type alias Model =
    { key : Browser.Navigation.Key
    , navState : Navbar.State
    , url : Url.Url
    , page : Page
    , diceModel : Page.Dice.Model
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg
    in
    ( { key = key
      , navState = navState
      , url = url
      , page = toPage url
      , diceModel = Page.Dice.init
      }
    , navCmd
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NavMsg Navbar.State
    | DiceMsg Page.Dice.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Browser.Navigation.load href
                    )

        DiceMsg diceMsg ->
            let
                ( diceModel, diceCmd ) =
                    Page.Dice.update diceMsg model.diceModel
            in
            ( { model | diceModel = diceModel }
            , Cmd.map DiceMsg diceCmd
            )

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        UrlChanged url ->
            ( { model | url = url, page = toPage url }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg



-- ROUTE


route : Url.Parser.Parser (Page -> a) a
route =
    Url.Parser.oneOf
        [ Url.Parser.map Home Url.Parser.top
        , Url.Parser.map Home (Url.Parser.s Base.base)
        , Url.Parser.map Dice (Url.Parser.s Base.base </> Url.Parser.s "dice")
        ]


toPage : Url.Url -> Page
toPage url =
    case Url.Parser.parse route url of
        Just answer ->
            answer

        Nothing ->
            NotFound



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "gipsond | elm-playground"
    , body =
        [ Html.div []
            [ menu model
            , Html.div [ Html.Attributes.class "container" ]
                -- Bootstrap container
                [ case model.page of
                    Home ->
                        Page.Home.view

                    Dice ->
                        Html.map DiceMsg (Page.Dice.view model.diceModel)

                    NotFound ->
                        notFoundView
                ]
            ]
        ]
    }


menu : Model -> Html.Html Msg
menu model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> internalLink Navbar.brand "gipsond | Elm Playground" "/"
        |> Navbar.items
            [ Navbar.dropdown
                { id = "apps"
                , toggle = Navbar.dropdownToggle [] [ Html.text "Apps" ]
                , items =
                    List.map
                        (uncurry <| internalLink <| Navbar.dropdownItem)
                        [ ( "Dice", "/dice" )
                        ]
                }
            , externalLink Navbar.itemLink "Source" "https://github.com/gipsond/elm-playground"
            ]
        |> Navbar.view model.navState


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


type alias ElementConstructor msg element =
    List (Html.Attribute msg) -> List (Html.Html msg) -> element


internalLink : ElementConstructor msg link -> String -> String -> link
internalLink linkConstructor label path =
    linkConstructor
        [ Html.Attributes.href <|
            Url.Builder.absolute [ Base.base, String.dropLeft 1 path ] []
        ]
        [ Html.text label ]


externalLink : ElementConstructor msg link -> String -> String -> link
externalLink linkConstructor label href =
    linkConstructor
        [ Html.Attributes.href href ]
        [ Octicons.linkExternal <| Octicons.defaultOptions
        , Html.text (" " ++ label)
        ]


notFoundView : Html.Html msg
notFoundView =
    Html.text "Not found"
