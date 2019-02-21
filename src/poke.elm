module Poke exposing (main)

-- HTML
-- App
-- HTTP | elm install elm/http
-- JSON | elm install NoRedInk/elm-json-decode-pipeline

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode exposing (Decoder, bool, field, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


type alias Pokemon =
    { name : String
    , id : Int
    -- , sprites : List String

    -- , pokeData : PokeData
    }


type alias PokeData =
    { number : Int
    , experience : Int
    , pokeType : PokeType
    , pokeMove : PokeMove
    }


type alias PokeType =
    { type1 : String
    , type2 : String
    }


type alias PokeMove =
    { move1 : String
    , move2 : String
    , move3 : String
    , move4 : String
    }


type alias Model =
    { pokeSelectedId : Int
    , pokeSelected : Pokemon
    }


type Msg
    = ClickedPokemon Int
    | GetPokemon (Result Http.Error Pokemon)
    -- | InitPokemon (Result Http.Error Pokemon)


initialModel : Model
initialModel =
    { pokeSelectedId = 140
    , pokeSelected = Pokemon "Kabuto" 140 --[]
    }


view model =
    div [ class "pokElm" ]
        [ div [ class "pokeHead" ] [ text "PokElm" ]
        , div [ class "pokeMenu" ] [ pokeDivList ]
        , div [ class "pokeMain" ] [ pokeDivInfo model ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPokemon pokeNumber ->
            ( model, getPokemon pokeNumber )

        GetPokemon (Ok pokemon) ->
            ( { model | pokeSelected = pokemon }, Cmd.none )

        GetPokemon (Err httpError) ->
            ( { model | pokeSelectedId = 66 }, Cmd.none )

        -- InitPokemon (Ok pokemon) ->
        --     ( { model | pokeSelectedId = 66 }, Cmd.none )

        -- InitPokemon (Err httpError) ->
        --     ( { model | pokeSelectedId = 66 }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


initialCmd : Cmd Msg
initialCmd =
    getPokemon 140


onCreatePokeDivItem : msg -> Attribute msg
onCreatePokeDivItem message =
    on "load" (Json.Decode.succeed message)


pokeDivList =
    div [ class "poke-div-list" ]
        (List.map pokeDivItem pokeNumsGen)


pokeDivItem pokeNumber =
    div [ class "poke-div-item", onClick (ClickedPokemon pokeNumber) ]
        [ text (String.fromInt pokeNumber) ]



-- [ text (getPokemon pokeNumber).name ]


getPokemon pokeNumber =
    Http.get
        { url = pokeAPI pokeNumber
        , expect = Http.expectJson GetPokemon pokeDecoder
        }


pokeDecoder : Decoder Pokemon
pokeDecoder =
    succeed Pokemon
        |> required "name" string
        |> required "id" int
        -- |> required "sprites" (list string)


pokeDivInfo model =
    div [ class "poke-div-info" ]
        [ div [ class "poke-div-name " ]
            [ text ("[" ++ String.fromInt model.pokeSelected.id ++ "] " ++ model.pokeSelected.name) ]
        , div [ class "poke-div-sprites" ]
            [ text "ola k ase" ]
            -- [ text model.pokeSelected.sprites ]
        ]


pokeAPI : Int -> String
pokeAPI pokeNumber =
    "https://pokeapi.co/api/v2/pokemon/" ++ String.fromInt pokeNumber


pokeNumsGen : List Int
pokeNumsGen =
    List.range 1 151
