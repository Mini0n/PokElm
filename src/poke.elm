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


type alias PokeItemAll =
    { pokes : List PokeItem
    }


type alias PokeItem =
    { name : String
    , url : String
    }


type alias Pokemon =
    { name : String
    , id : Int
    , sprites : PokeSprite

    -- , pokeData : PokeData
    }


type alias PokeSprite =
    { front : String
    , back : String
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
    , pokeListAll : PokeItemAll
    }


type Msg
    = ClickedPokemon Int
    | GetPokemon (Result Http.Error Pokemon)
    | GetPokemonAll (Result Http.Error PokeItemAll)



-- | InitPokemon (Result Http.Error Pokemon)


initialModel : Model
initialModel =
    { pokeSelectedId = 140
    , pokeSelected = Pokemon "Kabuto" 140 (PokeSprite "" "")
    , pokeListAll = PokeItemAll []
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

        GetPokemonAll (Ok pokemons) ->
            ( { model | pokeListAll = pokemons }, Cmd.none )

        GetPokemonAll (Err httpError) ->
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
    Cmd.none


pokeDivList =
    div [ class "poke-div-list" ]
        (List.map pokeDivItem pokeNumsGen)


pokeDivItem pokeNumber =
    div [ class "poke-div-item", onClick (ClickedPokemon pokeNumber) ]
        [ text (String.fromInt pokeNumber) ]


getPokemon pokeNumber =
    Http.get
        { url = pokeAPI pokeNumber
        , expect = Http.expectJson GetPokemon pokeDecoder
        }



-- getPokemonAll =
--     Http.get
--         { url = pokesAPIAll
--         , expect = Http.expectJson GetPokemonAll pokeDecoderAll
--         }


pokeDecoder : Decoder Pokemon
pokeDecoder =
    succeed Pokemon
        |> required "name" string
        |> required "id" int
        |> required "sprites" pokeSpriteDecoder


pokeSpriteDecoder : Decoder PokeSprite
pokeSpriteDecoder =
    succeed PokeSprite
        |> required "front_default" string
        |> required "back_default" string


pokeDivInfo model =
    div [ class "poke-div-info" ]
        [ div [ class "poke-div-name " ]
            [ text ("[" ++ pokePrintId model.pokeSelected.id ++ "] " ++ pokePrintName model.pokeSelected.name) ]
        , div [ class "poke-div-sprites" ]
            [ text model.pokeSelected.sprites.front ]
        ]


pokePrintId : Int -> String
pokePrintId pokeId =
    String.fromInt pokeId


pokePrintName : String -> String
pokePrintName pokeName =
    String.toUpper (String.slice 0 1 pokeName) ++ String.slice 1 (String.length pokeName) pokeName



-- pokeDecoderAll : Decoder PokeItemAll
-- pokeDecoderAll =
--     succeed PokeItemAll
--         |> required "result" list pokeItemDecoder
-- pokeItemDecoder : Decoder PokeItem
-- pokeItemDecoder =
--     succeed PokeItem
--         |> required "name" string


pokeAPI : Int -> String
pokeAPI pokeNumber =
    "https://pokeapi.co/api/v2/pokemon/" ++ String.fromInt pokeNumber


pokesAPIAll : String
pokesAPIAll =
    "https://pokeapi.co/api/v2/pokemon?limit=151"


pokeNumsGen : List Int
pokeNumsGen =
    List.range 1 151
