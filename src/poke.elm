module Poke exposing (main)

-- HTML
-- App
-- HTTP | elm install elm/http
-- JSON | elm install NoRedInk/elm-json-decode-pipeline

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, bool, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


type alias Pokemon =
    { number : Int
    , name : String
    , sprite : String
    , pokeData : PokeData
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
    { pokeNumsList : List Int
    , pokeSelected : Int
    }


type Msg
    = ClickedPokemon Int


initialModel : Model
initialModel =
    { pokeNumsList = pokeNumsGen
    , pokeSelected = 140
    }


view model =
    div [ class "pokElm" ]
        [ div [ class "pokeHead" ] [ text "PokElm" ]
        , div [ class "pokeMenu" ] [ pokeDivList ]
        , div [ class "pokeMain" ] [ text "pokes" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPokemon pokeNumber ->
            ( { model | pokeSelected = pokeNumber }, Cmd.none )


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
    Cmd.none


pokeDivList =
    div [ class "poke-div-list" ]
        (List.map pokeDivItem pokeNumsGen)



-- pokeDivItem : Int -> Html Html


pokeDivItem pokeNumber =
    div [ class "poke-div-item" ]
        [ text (String.fromInt pokeNumber) ]


pokeAPI : String -> String
pokeAPI pokeNumber =
    "https://pokeapi.co/api/v2/pokemon/" ++ pokeNumber


pokeNumsGen : List Int
pokeNumsGen =
    List.range 1 151
